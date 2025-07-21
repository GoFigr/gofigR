runtime_options <- structure(local({
  intercept_enabled=FALSE
  gofigr_options=NULL
  environment()
}))

#' Checks whether GoFigr intercept is on
#'
#' @returns TRUE if intercept is on, FALSE otherwise
#' @export
is_intercept_on <- function() {
  return(runtime_options$intercept_enabled)
}

intercept_on <- function() {
  runtime_options$intercept_enabled <- TRUE
}

intercept_off <- function() {
  runtime_options$intercept_enabled <- FALSE
}

#' Suppresses any automatic GoFigr publication hooks.
#'
#' @param func function in which to suppress intercepts
#'
#' @return the function with GoFigr supressed
#' @export
suppress <- function(func) {
 function(...) {
   initial_status <- is_intercept_on()

   tryCatch({
      intercept_off()
      func(...)
     }, finally={
       if(initial_status) {
         intercept_on()
       }
     })
 }
}

#' Checks whether GoFigr has been correctly configured.
#'
#' @param response function to use to show the warning message if not
#'   configured. Default: warning.
#'
#' @returns TRUE if configured, FALSE otherwise
#' @export
check_configured <- function(response=warning) {
  if(is.null(get_options())) {
    response("GoFigr not configured. Did you call enable()?")
    return(FALSE)
  }
  return(TRUE)
}

first_valid <- function(...) {
    xs <- unlist(list(...))
    xs <- xs[!is.null(xs)]
    if(is.null(xs) || length(xs) == 0) {
        return(NULL)
    } else {
        return(xs[1])
    }
}

#' Sets GoFigr options.
#'
#' @param options New options that will replace existing options.
#'
#' @return NA
#' @export
set_options <- function(options) {
  key <- get_rstudio_file_path()
  if(is.null(key)) {
    runtime_options$gofigr_options <- options
  } else {
    if(is.null(runtime_options$gofigr_options)) {
      runtime_options$gofigr_options <- list()
    }

    runtime_options$gofigr_options[[key]] <- options
  }
}

#' Gets configured GoFigr options.
#'
#' @return GoFigr options, or NULL if not set.
#' @export
get_options <- function() {
  key <- get_rstudio_file_path()
  if(is.null(key)) {
    return(runtime_options$gofigr_options)
  } else {
    return(runtime_options$gofigr_options[[key]])
  }
}

#' Gets the currently configured GoFigr client
#'
#' @returns GoFigr client
#' @export
get_client <- function() {
  opts <- get_options()
  return(opts$client)
}

is_debug_on <- function() {
  opts <- get_options()
  if(is.null(opts)) {
    return(FALSE)
  } else {
    return(opts$debug)
  }
}

debug_message <- function(...) {
  if(is_debug_on()) {
    message(...)
  }
}

#' Gets a title from a plot
#'
#' @param p plot object
#'
#' @return title or NULL
#' @export
get_title <- function(p) {
  if(is.null(p)) {
    return(NULL)
  } else if(inherits(p, "ggplot")) {
    return(p$labels$title)
  } else {
    return(NULL)
  }
}

print_revision <- function(rev, ...) {
  message(paste0(get_revision_url(rev), "\n"))
}


#' Wraps a plotting function (e.g. plot) so that its output is intercepted
#' by GoFigr.
#'
#' @param plot_func function to intercept
#'
#' @return intercepted function
#' @export
#'
#' @examples
#' gf_plot <- intercept(base::plot)
intercept <- function(plot_func) {
  function(...) {
    debug_message(paste0("Intercept called with ", paste0(lapply(list(...), class), collapse=", "),
                         ". Intercept: ", is_intercept_on()))

    if(!is_intercept_on()) {
      debug_message("Not publishing because intercept is off")
      return(plot_func(...))
    } else if(!check_configured()) {
      debug_message("Not publishing because not configured")
      return(plot_func(...))
    }

    tryCatch({
      plot_obj <- list(...)[[1]]

      # Check if the first argument is a plot we support. If not, defer to base_func
      if(!is_supported(plot_obj)) {
        debug_message(paste0("Ignoring figure because figure object ",
                             paste0(class(plot_obj), collapse=", "),
                             " is not supported"))

        return(plot_func(...))
      }

      publish(...)
    }, error=function(err) {
      warning(paste0("Error when publishing figure, will show original instead: ", err))
      plot_func(...)
    }, finally={
      intercept_on()
    })
  }
}

create_bare_revision <- function(client, fig, input_path, metadata) {
  sess <- utils::sessionInfo()
  info <- utils::capture.output({base::print(sess)})

  total_metadata <- list(input=input_path,
                         `getwd`=getwd(),
                         `R version`=paste0(info[[1]]),
                         `R details`=sess$R.version,
                         `Sys.info()`=as.list(Sys.info()),
                         `rsvg`=rsvg::librsvg_version())
  for(name in names(metadata)) {
    if("knitr_strict_list" %in% class(metadata[[name]])) {
      total_metadata[[name]] <- do.call(list, metadata[[name]])
    } else {
      total_metadata[[name]] <- metadata[[name]]
    }
  }

  rev <-gofigR::create_revision(client, fig,
                                metadata = total_metadata)

  return(rev)
}


apply_watermark <- function(rev_bare, plot_obj, gf_opts) {
  if(!is.null(gf_opts$watermark)) {
    watermarked_path <- tempfile(fileext=".png")
    qr <- gf_opts$watermark(rev_bare, NULL)
    p <- ggwatermark(qr, plot_obj)
    suppressMessages(ggplot2::ggsave(watermarked_path, plot=p))

    return(list(data_object=list(make_image_data("figure", watermarked_path, "png", TRUE)),
                png_path=watermarked_path))
  } else {
    return(NULL)
  }
}


capture_rds <- function(obj, name) {
  tmp_path <- tempfile()
  saveRDS(obj, tmp_path)
  res <- force(make_file_data(name, tmp_path))
  file.remove(tmp_path)
  return(res)
}


annotate <- function(rev_bare, plot_obj, figure_name,
                     source_path, source_contents, chunk_code=NULL,
                     custom_data=NULL) {
  sess <- utils::sessionInfo()
  info <- utils::capture.output({base::print(sess)})

  data <- list(make_text_data("sessionInfo", paste0(info, collapse="\n")))

  # Plot object as RDS file
  if(!is.null(plot_obj)) {
    data <- append(data, list(capture_rds(plot_obj,
                             paste0(default_if_null(figure_name, "plot_object"), ".RDS"))))
  }

  if(!is.null(custom_data)) {
    data <- append(data, list(capture_rds(custom_data, "data.RDS")))
  }

  if(!is.null(chunk_code)) {
    data <- append(data, list(make_code_data("Current chunk", chunk_code, "R")))
  }

  if(!is.null(source_path)) {
    data <- append(data, list(make_code_data("Input file", file(source_path),
                                             tools::file_ext(source_path))))
  }

  if(!is.null(source_contents) && trimws(source_contents) != "") {
    data <- append(data, list(make_code_data("Active document", source_contents,
                                             tools::file_ext(source_path))))
  }
  return(data)
}


save_as_image_file <- function(format, plot_obj) {
  path <- tempfile(fileext=paste0(".", format))
  p <- cowplot::ggdraw() + cowplot::draw_plot(plot_obj)
  suppressMessages(ggplot2::ggsave(path, plot=p))
  return(path)
}

save_as_image <- function(format, plot_obj) {
  path <- save_as_image_file(format, plot_obj)
  img_obj <- make_image_data("figure", path, format, FALSE)
  file.remove(path)
  return(img_obj)
}

check_show_setting <- function(show) {
  if(is.null(show) || !show %in% list("watermark", "original")) {
    stop(paste0("Valid show values are watermark or original, but you passed: ", show))
  }
}

display <- function(rev, plot_obj) {
  gf_opts <- get_options()
  show_mode <- gf_opts$show

  debug_message(paste0("Preparing to display figure. Revision: ",
                       if(!is.null(rev)) rev$api_id else "NULL",
                       ". Show: ", show_mode,
                       ". Watermark not null: ", !is.null(gf_opts$watermark)))

  if(!is.null(rev) && show_mode == "watermark" && !is.null(gf_opts$watermark)) {
    debug_message("Showing watermark")

    qr <- gf_opts$watermark(rev, NULL)

    base::plot(ggwatermark(qr, plot_obj))
  } else {
    debug_message("Showing original")

    base::plot(plot_obj)
  }
}


#' Executes an expression while isolating any new graphics devices it creates.
#'
#' @param expr The R expression to evaluate.
#' @return result of evaluating expr
#' @export
with_isolated_devices <- function(expr) {
  # 1. Backup the original device state
  original_devices <- grDevices::dev.list()
  original_active_device <- tryCatch(grDevices::dev.cur(), error = function(e) NULL)

  # 2. Schedule the cleanup to run when the function exits (even on error)
  on.exit({
    # Get the list of devices after the expression has run
    new_devices <- grDevices::dev.list()

    # Identify any devices that were opened by the expression
    devices_to_close <- setdiff(new_devices, original_devices)

    # Close them
    for (dev in devices_to_close) {
      grDevices::dev.off(dev)
    }

    # Restore the originally active device, if it still exists
    if (!is.null(original_active_device) && original_active_device %in% grDevices::dev.list()) {
      grDevices::dev.set(original_active_device)
    }
  })

  # 3. Evaluate the user's expression
  force(expr)
}


#' Tries to convert expression to a grob, returning it unchanged if it fails.
#'
#' @param expr expression/object to convert
#'
#' @returns grob if successful, expr if not
#' @export
try_base2grob <- function(expr) {
  as_gg <- NULL

  tryCatch({
    with_isolated_devices({
      as_gg <- ggplotify::as.ggplot(ggplotify::base2grob(function() {
        expr
    }))})
  }, error=function(err) {
    as_gg <<- expr
  })

  return(as_gg)
}

#' Captures output from grid graphics (ggplot2, lattice, ComplexHeatmap, etc.)
#' and publishes it to GoFigr.
#'
#' @param expr the expression to plot
#' @param ... passed through to publish()
#'
#' @return GoFigr Revision object
#' @export
publish_base <- function(expr, ...) {
  .Deprecated("publish")

  with_isolated_devices({
    asgg <- ggplotify::as.ggplot(ggplotify::base2grob(function() {
        expr
      }))

    publish(asgg, ..., base_convert=FALSE)
  })
}


#' Publishes a figure to the GoFigr service.
#'
#' @param plot_obj plot to publish
#' @param figure_name name of the figure. If NULL, it will be inferred from the figure's title
#' @param input_path path to the source file
#' @param input_contents contents of the source file
#' @param chunk_code chunk code, if running R markdown
#' @param image_formats image formats to save
#' @param data optional data to save with this figure. The data will be saved as RDS.
#' @param metadata optional metadata
#' @param show whether to display the figure after publication
#' @param base_convert whether to try converting base graphics to grid graphics
#'
#' @returns GoFigr revision object
#' @export
publish <- function(plot_obj,
                    figure_name=NULL,
                    input_path=NULL,
                    input_contents=NULL,
                    chunk_code=NULL,
                    image_formats=c("eps"),
                    data=NULL,
                    metadata=NULL,
                    show=TRUE,
                    base_convert=TRUE) {
  gf_opts <- get_options()
  if(is.null(gf_opts)) {
    warning("GoFigr hasn't been configured. Did you call gofigR::enable()?")
    return(invisible(NULL))
  }

  if(base_convert) {
    res <- try_base2grob(plot_obj)
    if(is_supported(res)) {
      plot_obj <- res
    }
  }

  plot_obj <- to_ggplot(plot_obj)
  if(is.null(plot_obj)) {
    warning(paste0("Provided object cannot be converted to ggplot: ", plot_obj))
    return(invisible(NULL))
  }

  context <- get_execution_context()
  figure_name <- first_valid(figure_name, get_title(plot_obj))
  input_path <- first_valid(input_path, context$input_path)
  input_contents <- first_valid(input_contents, context$input_contents)
  chunk_code <- first_valid(chunk_code, context$chunk_code)

  if(is.null(metadata)) {
    metadata <- context$metadata
  }

  if(is.null(figure_name)) {
    figure_name <- "Anonymous Figure"
    warning("Your figure lacks a name and will be published as Anonymous Figure.")
  }


  debug_message(paste0("Starting publish. Devices: ",
                       paste0(names(grDevices::dev.list()), collapse=", ")))

  client <- gf_opts$client

  png_path <- save_as_image_file("png", plot_obj)
  if(!file.exists(png_path)) {
    if(gf_opts$verbose) {
      warning("No output to publish\n")
    }
    return()
  }

  fig <- gofigR::find_figure(gf_opts$client, gf_opts$analysis, figure_name, create=TRUE)

  # Create a bare revision to get API ID
  rev_bare <- create_bare_revision(client, fig, input_path, metadata)

  # Now that we have an API ID, apply the watermark
  image_data <- list(make_image_data("figure", png_path, "png", FALSE))
  watermark_data <- apply_watermark(rev_bare, plot_obj, gf_opts)
  if(!is.null(watermark_data)) {
    image_data <- append(image_data, watermark_data$data_object)
  }

  # Additional image formats
  image_data <- append(image_data, lapply(image_formats, function(fmt) {
    save_as_image(fmt, plot_obj)
  }))
  image_data <- image_data[!is.null(image_data)]

  other_data <- annotate(rev_bare, plot_obj, fig$name, input_path, input_contents, chunk_code, data)

  rev <- gofigR::update_revision_data(client, rev_bare, silent=TRUE,
                                      new_data=append(image_data, other_data),
                                      assets=unname(gf_opts$assets))
  file.remove(png_path)

  if(gf_opts$verbose) {
    message(paste0("\"", fig$name, "\" at ", get_revision_url(rev), "\n"))
  }

  debug_message(paste0("Ending publish. Devices: ", paste0(names(grDevices::dev.list()), collapse=", ")))

  if(show) {
    display(rev, plot_obj)
  }

  class(rev) <- "gofigr_revision"
  rev$client <- gf_opts$client

  return(rev)
}

#' Default print method for GoFigr revisions.
#'
#' @param x revision
#' @param ... passed to base::print
#'
#' @returns NA
#' @export
print.gofigr_revision <- function(x, ...) {
  print(get_revision_url(x), ...)
}

#' Default cat method for GoFigr revisions.
#'
#' @param x revision
#' @param ... passed to cat
#'
#' @returns NA
#' @export
cat.gofigr_revision <- function(x, ...) {
  cat(paste0(get_revision_url(x), "\n"), ...)
}

to_ggplot <- function(x, warn = FALSE) {
  if (is_ggplot(x)) {
    return(x)
  }

  converted <- NULL
  tryCatch({
    converted <- ggplotify::as.ggplot(x)
  }, error = function(err) {
    if (warn) {
      warning(err)
    }
    converted <- NULL
  })
  return(converted)
}

is_supported <- function(x) {
  return(!is.null(to_ggplot(x)))
}

is_ggplot <- function(x) {
  return(any(class(x) == "ggplot"))
}

make_invisible <- function(func) {
  function(...) {
    return(invisible(func(...)))
  }
}

#' Plots and publishes an object (if supported)
#' @param ... passed directly to plot
#' @returns result of the call to plot(...)
#'
#' @export
gf_plot <- make_invisible(intercept(base::plot))

#' Prints and publishes an object (if supported)
#' @param ... passed directly to print
#' @returns result of the call to print(...)
#'
#' @export
gf_print <- make_invisible(intercept(base::print))

intercept_base <- function(env=.GlobalEnv) {
  assign("print", gf_print, env)
}


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#'
#' @returns null
#' @export
sync_file <- function(path) {
  if(check_configured()) {
    opts <- get_options()
    asset_rev <- sync_workspace_asset(opts$client, opts$workspace, path)
    message(paste0("Asset synced: ", APP_URL, "/asset_revision/", asset_rev$api_id))
    opts$assets[[get_api_id(asset_rev)]] <- asset_rev
  }
  return(path)
}

wrap_reader <- function(reader) {
  function(path, ...) {
    reader(sync_file(path), ...)
  }
}

#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to utils::read.csv
#'
#' @returns data frame
#' @export
read.csv <- wrap_reader(utils::read.csv)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to utils::read.csv
#'
#' @returns data frame
#' @export
read.csv2 <- wrap_reader(utils::read.csv2)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to readr::read_csv
#'
#' @returns data frame
#' @export
read_csv <- wrap_reader(readr::read_csv)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to readr::read_csv2
#'
#' @returns data frame
#' @export
read_csv2 <- wrap_reader(readr::read_csv2)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to readr::read_tsv
#'
#' @returns data frame
#' @export
read_tsv <- wrap_reader(readr::read_tsv)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to readr::read_delim
#'
#' @returns data frame
#' @export
read_delim <- wrap_reader(readr::read_delim)


#' Syncs a file with the GoFigr service and stores a reference. The file
#' will be associated with all figures published after this call.
#'
#' @param path path to the file
#' @param ... passed to openxlsx::read.xlsx
#'
#' @returns data frame
#' @export
read.xlsx <- wrap_reader(openxlsx::read.xlsx)


#' Enables GoFigr in the current R/Rmd file.
#'
#' @param auto_publish will publish all plots automatically if TRUE. Note
#'  that setting this option will re-assign plot() and print() in the global environment.
#' @param analysis_api_id Analysis API ID (if analysis_name is NULL)
#' @param analysis_name Analysis name (if analysis_api_id is NULL)
#' @param workspace API ID of the workspace
#' @param create_analysis if TRUE and analysis_name does not exist, it will be automatically created
#' @param analysis_description analysis description if creating a new analysis
#' @param watermark watermark class to use, e.g. QR_WATERMARK, LINK_WATERMARK or NO_WATERMARK
#' @param verbose whether to show verbose output
#' @param debug whether to show debugging information
#' @param show which figure to display in the document: original, watermark, or hide. Note that this setting \
#' only affects the display and doesn't change what gets published: e.g. even if you choose to display \
#' the original figure, the watermarked version will still be published to GoFigr.
#' @param api_key GoFigr API key
#'
#' @return named list of GoFigr options
#' @export
enable <- function(auto_publish=FALSE,
                   analysis_api_id=NULL,
                   analysis_name=NULL,
                   workspace=NULL,
                   create_analysis=TRUE,
                   analysis_description=NULL,
                   watermark=QR_WATERMARK,
                   verbose=FALSE,
                   debug=FALSE,
                   api_key=NULL,
                   show="watermark") {
  check_show_setting(show)

  context <- get_execution_context()
  if(is.null(analysis_name) && !is.null(context$input_path)) {
    analysis_name <- basename(context$input_path)
    message(paste0("Analysis name: ", analysis_name))
  }

  # Create the GoFigr client
  gf <- gofigr_client(workspace = workspace,
                      verbose = verbose,
                      api_key = api_key)

  # Find the analysis
  if(!is.null(analysis_api_id)) {
    ana <- gofigR::get_analysis(analysis_api_id)
  } else if(!is.null(analysis_name)) {
    ana <- gofigR::find_analysis(gf, analysis_name,
                                 workspace = gf$workspace,
                                 create = create_analysis,
                                 description = analysis_description)
  } else {
    stop("Please specify either analysis_api_id or analysis_name")
  }

  set_options(structure(local({
    client <- gf
    analysis <- ana
    workspace <- gf$workspace
    watermark <- watermark
    verbose <- verbose
    debug <- debug
    show <- show
    assets <- list()

    return(environment())
  })))

  intercept_on()
  if(auto_publish) {
    intercept_base()
  }

  return(invisible(get_options()))
}
