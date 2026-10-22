# Jupyter (IRkernel) integration: automatic publishing and rich output.
#
# RStudio has no "plot finished" event, which is why auto-publish there was
# deprecated. IRkernel does: it runs every cell through evaluate(), whose
# output handler is called once per completed plot (a recordedplot, after
# incremental base-graphics calls have been merged) and once per visible
# value (an unprinted ggplot object). enable(auto_publish = TRUE) hooks
# those two points on the running kernel's Executor:
#
#   handle_value(obj, visible) -- a visible plot object is published
#       instead of being rendered by the kernel; everything else goes to
#       the original handler.
#   send_plot(plotobj) -- the completed recordedplot (base graphics, or a
#       ggplot the cell print()ed) is published instead of being sent as
#       a plain image.
#
# publish() then displays the figure itself (jupyter_display_revision):
# one display_data carrying image/png (the watermarked PNG, what a plain
# Jupyter frontend and an .ipynb export show), text/html (that image plus
# the published-revision widget) and application/vnd.gofigr.figure+json
# (the revision's identity, the raw PNG and the watermark parameters, which
# the GoFigr canvas renders natively). The payload contract is shared with
# gofigr-python's mime.py (version 1).

GOFIGR_FIGURE_MIME <- "application/vnd.gofigr.figure+json"
GOFIGR_FIGURE_PAYLOAD_VERSION <- 1L
# The canvas stores run outputs with a cap on embedded images; anything
# bigger is referenced, not embedded (same limit as the Python client).
MAX_EMBEDDED_IMAGE_BYTES <- 1400000L

# Plot classes the value hook publishes. Deliberately a whitelist: ggplotify
# can also "plot" functions and formulas by calling them, which is not what
# a visible function value means.
JUPYTER_PLOT_CLASSES <- c("ggplot", "gg", "ggsurvplot", "gtable", "grob", "trellis",
                          "Heatmap", "HeatmapList", "recordedplot")

jupyter_state <- new.env(parent = emptyenv())
jupyter_state$hooks_installed <- FALSE
jupyter_state$publishing <- FALSE

#' Sends a raw mime bundle to the Jupyter frontend. IRkernel installs the
#' base display function as an option; IRdisplay is a thin wrapper over it.
#'
#' @param data named list of mime type -> content
#' @param metadata optional named list of mime type -> metadata
#' @return TRUE if the bundle was sent
#' @noRd
jupyter_publish_mimebundle <- function(data, metadata = NULL) {
  sender <- getOption("jupyter.base_display_func")
  if (is.function(sender)) {
    sender(data, metadata)
    return(TRUE)
  }
  if (requireNamespace("IRdisplay", quietly = TRUE)) {
    IRdisplay::publish_mimebundle(data, metadata)
    return(TRUE)
  }
  FALSE
}

#' Displays an HTML fragment in the notebook (the setup/startup widget).
#'
#' @param html HTML string
#' @param text plain-text fallback
#' @return TRUE if displayed
#' @noRd
jupyter_display_html <- function(html, text = "") {
  jupyter_publish_mimebundle(list("text/html" = html, "text/plain" = text))
}

#' Finds the IRkernel Executor running the current cell by walking the call
#' stack: reference-class methods close over their object as `.self`.
#'
#' @return the Executor, or NULL when not called from inside a cell
#' @noRd
find_jupyter_executor <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    env <- environment(sys.function(i))
    if (is.null(env)) next
    self <- get0(".self", envir = env, inherits = FALSE)
    if (is.null(self)) next
    if (inherits(self, "Executor")) return(self)
    if (inherits(self, "Kernel") && !is.null(self$executor)) return(self$executor)
  }
  NULL
}

#' Plot dimensions for a published figure: what IRkernel would have used
#' to render it (repr.plot.* options, stamped on the recordedplot by the
#' kernel).
#'
#' @param plotobj a recordedplot carrying .irkernel_* attributes, or NULL
#' @return list(width, height) in inches and dpi
#' @noRd
jupyter_plot_dims <- function(plotobj = NULL) {
  list(width = default_if_null(attr(plotobj, ".irkernel_width"), getOption("repr.plot.width", 7)),
       height = default_if_null(attr(plotobj, ".irkernel_height"), getOption("repr.plot.height", 7)),
       dpi = default_if_null(attr(plotobj, ".irkernel_ppi"), getOption("repr.plot.res", 120)))
}

is_jupyter_plot <- function(obj) {
  inherits(obj, JUPYTER_PLOT_CLASSES)
}

#' Publishes a plot the kernel is about to display, if auto-publish is on.
#'
#' @param obj plot object or recordedplot
#' @param dims list(width, height, dpi) as from jupyter_plot_dims()
#' @return TRUE if the plot was published (and displayed by publish()),
#'   FALSE if the kernel should display it as usual
#' @noRd
jupyter_auto_publish <- function(obj, dims) {
  opts <- get_options()
  if (is.null(opts) || !isTRUE(opts$auto_publish) || !is_intercept_on() || jupyter_state$publishing) {
    return(FALSE)
  }
  if (!is_jupyter_plot(obj)) {
    return(FALSE)
  }
  jupyter_state$publishing <- TRUE
  on.exit(jupyter_state$publishing <- FALSE)

  tryCatch({
    if (inherits(obj, "recordedplot")) {
      # Base graphics: replay the recording into a grob (gridGraphics echoes
      # what the function draws). Skips publish()'s own base2grob step,
      # which would evaluate the recordedplot object without drawing it.
      recording <- obj
      obj <- with_isolated_devices(
        ggplotify::as.ggplot(ggplotify::base2grob(function() grDevices::replayPlot(recording)))
      )
      publish(obj, width = dims$width, height = dims$height, units = "in", dpi = dims$dpi,
              base_convert = FALSE)
    } else {
      publish(obj, width = dims$width, height = dims$height, units = "in", dpi = dims$dpi)
    }
    TRUE
  }, error = function(err) {
    warning(paste0("GoFigr could not auto-publish this figure, showing it unpublished instead: ",
                   conditionMessage(err)))
    FALSE
  })
}

#' Installs the auto-publish hooks on the running IRkernel Executor.
#'
#' Methods of a reference-class object are looked up in the object's
#' environment first, so binding a function of the same name there
#' overrides the method for every later call, including the Executor's own
#' unqualified calls (execute() -> send_plot()). The originals are kept and
#' called for anything that is not auto-published.
#'
#' @param executor the Executor (or its environment, for tests). Defaults
#'   to the one running the current cell.
#' @return TRUE if the hooks are installed
#' @noRd
install_jupyter_hooks <- function(executor = find_jupyter_executor()) {
  if (jupyter_state$hooks_installed) {
    return(TRUE)
  }
  if (is.null(executor)) {
    warning("GoFigr auto-publish is only available inside a Jupyter cell (IRkernel); ",
            "call gofigR::publish() explicitly.")
    return(FALSE)
  }
  env <- if (is.environment(executor)) executor else as.environment(executor)
  original_send_plot <- get0("send_plot", envir = env)
  original_handle_value <- get0("handle_value", envir = env)
  if (!is.function(original_send_plot) || !is.function(original_handle_value)) {
    warning("GoFigr auto-publish: unsupported IRkernel version (no send_plot/handle_value); ",
            "call gofigR::publish() explicitly.")
    return(FALSE)
  }

  assign("send_plot", function(plotobj) {
    if (!jupyter_auto_publish(plotobj, jupyter_plot_dims(plotobj))) {
      original_send_plot(plotobj)
    }
  }, envir = env)

  assign("handle_value", function(obj, visible) {
    if (visible && jupyter_auto_publish(obj, jupyter_plot_dims())) {
      # The original also records .Last.value; keep that behaviour.
      if (requireNamespace("IRkernel", quietly = TRUE)) {
        set_last <- get0("set_last_value", envir = asNamespace("IRkernel"), inherits = FALSE)
        if (is.function(set_last)) tryCatch(set_last(obj), error = function(e) NULL)
      }
      return(invisible(NULL))
    }
    original_handle_value(obj, visible)
  }, envir = env)

  jupyter_state$hooks_installed <- TRUE
  TRUE
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

WIDGET_STYLE <- paste0("display: flex; align-items: center; gap: 0.5rem; flex-wrap: wrap; ",
                       "font-family: sans-serif; font-size: 0.9rem; margin: 0.25rem 0 0.5rem 0; ",
                       "padding: 0.4rem 0.6rem; border: 1px solid #dfe3e8; border-radius: 6px;")
LINK_STYLE <- "color: #0d53b1; text-decoration: none;"

widget_link <- function(url, label) {
  sprintf("<a href=\"%s\" target=\"_blank\" rel=\"noopener\" style=\"%s\">%s</a>",
          html_escape(url), LINK_STYLE, html_escape(label))
}

#' The published-revision widget shown under a figure in Jupyter.
#'
#' @param rev revision (with api_id/short_id)
#' @param figure_name figure name
#' @return HTML string
#' @noRd
revision_widget_html <- function(rev, figure_name) {
  url <- get_revision_url(rev)
  sprintf(paste0("<div style=\"%s\"><span style=\"font-weight: bold\">GoFigr</span>",
                 "<span>Published <b>%s</b></span>%s</div>"),
          WIDGET_STYLE, html_escape(default_if_null(figure_name, "figure")),
          if (is.null(url)) "" else widget_link(url, "View on GoFigr"))
}

#' The startup widget shown by enable() in Jupyter.
#'
#' @param workspace workspace object (api_id, name)
#' @param analysis analysis object (api_id, name)
#' @param auto_publish whether auto-publish is on
#' @param client GoFigr client (for the app URL)
#' @return HTML string
#' @noRd
startup_widget_html <- function(workspace, analysis, auto_publish, client = NULL) {
  app <- get_app_url(client)
  sprintf(paste0("<div style=\"%s\"><span><span style=\"font-weight: bold\">GoFigr active</span>. ",
                 "Workspace: %s. Analysis: %s. Auto-publish: %s.</span></div>"),
          WIDGET_STYLE,
          widget_link(paste0(app, "/workspace/", workspace$api_id), default_if_null(workspace$name, "workspace")),
          widget_link(paste0(app, "/analysis/", analysis$api_id), default_if_null(analysis$name, "analysis")),
          if (isTRUE(auto_publish)) "TRUE" else "FALSE")
}

png_dimensions <- function(path) {
  tryCatch({
    info <- magick::image_info(magick::image_read(path))
    list(width = as.integer(info$width[1]), height = as.integer(info$height[1]))
  }, error = function(e) list(width = NULL, height = NULL))
}

#' The application/vnd.gofigr.figure+json payload for a published revision
#' (version 1, see gofigr-python's mime.py).
#'
#' @param rev revision as returned by the server
#' @param figure_name figure name
#' @param raw_png_path path to the unwatermarked PNG
#' @param watermark_enabled whether a watermark was published
#' @param image_formats other image formats published (besides png)
#' @return list, ready for jsonlite
#' @noRd
figure_payload <- function(rev, figure_name, raw_png_path, watermark_enabled, image_formats = character(0)) {
  url <- get_revision_url(rev)
  figure <- rev$figure
  figure_api_id <- if (is.list(figure)) figure$api_id else figure

  image <- NULL
  if (!is.null(raw_png_path) && file.exists(raw_png_path)) {
    bytes <- read_raw_bytes(raw_png_path)
    dims <- png_dimensions(raw_png_path)
    embedded <- length(bytes) <= MAX_EMBEDDED_IMAGE_BYTES
    image <- list(format = "png", width = dims$width, height = dims$height,
                  b64 = if (embedded) base64enc::base64encode(bytes) else NULL,
                  embedded = embedded)
  }

  formats <- list(list(format = "png", watermarked = FALSE))
  if (watermark_enabled) formats <- append(formats, list(list(format = "png", watermarked = TRUE)))
  for (fmt in image_formats) formats <- append(formats, list(list(format = tolower(fmt), watermarked = FALSE)))

  list(
    version = GOFIGR_FIGURE_PAYLOAD_VERSION,
    revision = list(api_id = rev$api_id, short_id = rev$short_id, url = url,
                    created_on = rev$created_on, size_bytes = rev$size_bytes),
    figure = list(api_id = figure_api_id, name = figure_name),
    watermark = if (watermark_enabled && !is.null(url)) {
      list(url = url, qr = TRUE, margin_px = c(10L, 10L), qr_scale = 2L, font_px = 14L)
    } else NULL,
    image = image,
    interactive = NULL,
    formats = formats
  )
}

#' Displays a just-published figure in Jupyter as one rich output.
#'
#' @param rev revision as returned by the server
#' @param figure_name figure name
#' @param raw_png_path path to the unwatermarked PNG
#' @param watermarked_png_path path to the watermarked PNG, or NULL
#' @param image_formats other image formats published
#' @return TRUE if displayed
#' @noRd
jupyter_display_revision <- function(rev, figure_name, raw_png_path, watermarked_png_path = NULL,
                                     image_formats = character(0)) {
  shown_path <- if (!is.null(watermarked_png_path) && file.exists(watermarked_png_path)) watermarked_png_path else raw_png_path
  if (is.null(shown_path) || !file.exists(shown_path)) {
    return(FALSE)
  }
  shown_b64 <- base64enc::base64encode(read_raw_bytes(shown_path))
  payload <- figure_payload(rev, figure_name, raw_png_path,
                            watermark_enabled = !is.null(watermarked_png_path), image_formats = image_formats)
  url <- get_revision_url(rev)
  data <- list(
    "image/png" = shown_b64,
    "text/html" = paste0("<img src=\"data:image/png;base64,", shown_b64, "\" style=\"max-width: 100%; height: auto;\"/>",
                         revision_widget_html(rev, figure_name)),
    "text/plain" = paste0("<GoFigr figure ", default_if_null(url, ""), ">")
  )
  data[[GOFIGR_FIGURE_MIME]] <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))
  jupyter_publish_mimebundle(data)
}
