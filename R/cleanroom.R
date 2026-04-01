# ---- Parameter constructors ----

#' Creates a slider parameter for use in \code{\link{reproducible}} functions.
#'
#' @param default default value (integer or numeric)
#' @param min minimum value
#' @param max maximum value
#' @param step step size
#'
#' @return a \code{gf_param} object
#' @export
slider <- function(default, min = NULL, max = NULL, step = NULL) {
  type <- if (is.integer(default)) "integer" else "number"
  structure(list(
    default = default,
    type = type,
    widget = "slider",
    min = min,
    max = max,
    step = step
  ), class = "gf_param")
}

#' Creates a dropdown parameter for use in \code{\link{reproducible}} functions.
#'
#' @param default default value
#' @param choices character vector of allowed values
#'
#' @return a \code{gf_param} object
#' @export
dropdown <- function(default, choices) {
  structure(list(
    default = default,
    type = "string",
    widget = "dropdown",
    choices = choices
  ), class = "gf_param")
}

#' Creates a checkbox parameter for use in \code{\link{reproducible}} functions.
#'
#' @param default default logical value
#'
#' @return a \code{gf_param} object
#' @export
checkbox <- function(default) {
  structure(list(
    default = default,
    type = "boolean",
    widget = "checkbox"
  ), class = "gf_param")
}

#' Creates a text input parameter for use in \code{\link{reproducible}} functions.
#'
#' @param default default character value
#'
#' @return a \code{gf_param} object
#' @export
text_input <- function(default) {
  structure(list(
    default = default,
    type = "string",
    widget = "text"
  ), class = "gf_param")
}

infer_type <- function(value) {
  if (is.data.frame(value)) return("dataframe")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("number")
  if (is.logical(value)) return("boolean")
  if (is.character(value)) return("string")
  return("none")
}

#' Creates a static (read-only) parameter for use in \code{\link{reproducible}} functions.
#'
#' Use this for data frames and other non-interactive values.
#'
#' @param default the value
#'
#' @return a \code{gf_param} object
#' @export
static <- function(default) {
  structure(list(
    default = default,
    type = infer_type(default)
  ), class = "gf_param")
}


# ---- Manifest building ----

param_to_manifest_entry <- function(param) {
  entry <- list(type = jsonlite::unbox(param$type))

  if (!is.null(param$widget)) {
    entry$widget <- jsonlite::unbox(param$widget)
  }

  # Value: omit for dataframes (they're stored separately as Parquet)
  if (param$type != "dataframe") {
    entry$value <- jsonlite::unbox(param$default)
  }

  # Slider bounds
  if (!is.null(param$min)) entry$min <- jsonlite::unbox(param$min)
  if (!is.null(param$max)) entry$max <- jsonlite::unbox(param$max)
  if (!is.null(param$step)) entry$step <- jsonlite::unbox(param$step)

  # Dropdown choices
  if (!is.null(param$choices)) entry$choices <- param$choices

  entry
}

build_manifest <- function(descriptors, packages, imports, name) {
  params <- list()
  for (nm in names(descriptors)) {
    params[[nm]] <- param_to_manifest_entry(descriptors[[nm]])
  }

  # Resolve package versions
  pkg_versions <- list()
  if (is.character(packages) && !is.null(names(packages))) {
    # Named character vector: names are packages, values are versions
    pkg_versions <- lapply(as.list(packages), jsonlite::unbox)
  } else if (is.character(packages)) {
    # Unnamed character vector: resolve versions
    for (pkg in packages) {
      pkg_versions[[pkg]] <- jsonlite::unbox(as.character(utils::packageVersion(pkg)))
    }
  } else if (is.list(packages)) {
    # Named list with explicit versions
    pkg_versions <- lapply(packages, jsonlite::unbox)
  }

  manifest <- list(
    language = jsonlite::unbox("r"),
    language_version = jsonlite::unbox(paste0(R.version$major, ".", R.version$minor)),
    function_name = if (is.null(name)) jsonlite::unbox(NA) else jsonlite::unbox(name),
    packages = pkg_versions,
    imports = if (length(imports) == 0) structure(list(), names = character(0)) else imports,
    parameters = params
  )

  manifest
}


# ---- Clean room data attachment ----

build_clean_room_data <- function(context) {
  data_objects <- list()

  # 1. Code data: function source with clean_room format
  code_metadata <- list(format = "clean_room")
  if (!is.null(context$name)) {
    code_metadata$function_name <- context$name
  }
  code_obj <- make_code_data("clean_room_code", context$source, "R",
                             format = "clean_room", metadata = code_metadata)
  code_obj$is_clean_room <- TRUE
  data_objects <- append(data_objects, list(code_obj))

  # 2. Manifest as text data
  manifest_json <- jsonlite::toJSON(context$manifest, auto_unbox = FALSE,
                                     null = "null", pretty = FALSE)
  manifest_obj <- make_text_data("clean_room_manifest", as.character(manifest_json),
                                 metadata = list(role = "manifest"))
  manifest_obj$is_clean_room <- TRUE
  data_objects <- append(data_objects, list(manifest_obj))

  # 3. DataFrame parameters as Parquet
  for (nm in names(context$descriptors)) {
    desc <- context$descriptors[[nm]]
    if (desc$type == "dataframe" && is.data.frame(desc$default)) {
      parquet_path <- tempfile(fileext = ".parquet")
      nanoparquet::write_parquet(desc$default, parquet_path)
      parquet_bytes <- readBin(parquet_path, "raw", file.info(parquet_path)$size)
      file.remove(parquet_path)

      table_obj <- make_raw_data(nm, DATA_TYPES$data_frame,
                                 metadata = list(format = "parquet"),
                                 data = parquet_bytes)
      class(table_obj) <- "gofigrdata"
      table_obj$is_clean_room <- TRUE
      data_objects <- append(data_objects, list(table_obj))
    }
  }

  data_objects
}


# ---- Source capture ----

capture_function_source <- function(fn) {
  # Extract only the function body via R's AST (not string manipulation).
  # body(fn) returns a `{` call whose elements 2..n are the statements.
  # We deparse each statement individually, matching Python's AST approach
  # of stripping the def/decorator lines.
  body_expr <- body(fn)

  # body() of a single-expression function is just that expression (no `{`)
  if (!is.call(body_expr) || !identical(body_expr[[1]], as.symbol("{"))) {
    return(paste(deparse(body_expr), collapse = "\n"))
  }

  # Multi-statement body: skip the `{` symbol at position 1
  statements <- vapply(2:length(body_expr), function(i) {
    paste(deparse(body_expr[[i]]), collapse = "\n")
  }, character(1))

  paste(statements, collapse = "\n")
}


# ---- Main entry point ----

#' Execute a function in a clean room environment and publish with full reproducibility metadata.
#'
#' The function's formal parameters should use \code{\link{slider}}, \code{\link{dropdown}},
#' \code{\link{checkbox}}, \code{\link{text_input}}, or \code{\link{static}} to declare
#' parameter types. Call \code{\link{publish}} inside the function body to publish figures
#' with clean room metadata attached.
#'
#' @param fn a function whose formals use \code{gf_param} defaults
#' @param packages character vector of package names, or named list with versions
#'   (e.g. \code{list(ggplot2 = "3.5.0")}). Package versions are resolved automatically
#'   if not provided.
#' @param imports named list mapping aliases to package names (e.g.
#'   \code{list(plt = "ggplot2")}). Optional.
#' @param name optional display name for the function (shown in the webapp)
#' @param interactive if TRUE, launches a Shiny gadget in the RStudio Viewer pane
#'   with parameter widgets for interactive exploration. The user can adjust parameters
#'   and re-run the figure live, then click Publish to publish with clean room metadata.
#'
#' @return whatever the function body returns (typically the result of \code{publish()})
#' @export
reproducible <- function(fn, packages = character(0), imports = list(), name = NULL,
                         interactive = FALSE) {
  # Check for nanoparquet

  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    warning("The 'nanoparquet' package is required for clean room support. ",
            "Install it with install.packages('nanoparquet'). ",
            "Running without clean room metadata.")
    # Execute function normally with unwrapped params
    resolved <- list()
    for (nm in names(formals(fn))) {
      val <- formals(fn)[[nm]]
      evaluated <- tryCatch(eval(val), error = function(e) val)
      if (inherits(evaluated, "gf_param")) {
        resolved[[nm]] <- evaluated$default
      } else {
        resolved[[nm]] <- evaluated
      }
    }
    return(do.call(fn, resolved))
  }

  # Step 1: Extract parameter descriptors from formals
  fmls <- formals(fn)
  descriptors <- list()
  resolved_params <- list()

  for (nm in names(fmls)) {
    val <- fmls[[nm]]
    # Evaluate the default expression to get the gf_param or plain value
    evaluated <- tryCatch(eval(val), error = function(e) val)

    if (inherits(evaluated, "gf_param")) {
      descriptors[[nm]] <- evaluated
      resolved_params[[nm]] <- evaluated$default
    } else {
      # Wrap plain defaults in static()
      descriptors[[nm]] <- static(evaluated)
      resolved_params[[nm]] <- evaluated
    }
  }

  # Step 3: Capture source code
  source_code <- capture_function_source(fn)

  # Step 4 & 5: Build manifest
  manifest <- build_manifest(descriptors, packages, imports, name)

  # Build context
  context <- list(
    source = source_code,
    name = name,
    manifest = manifest,
    descriptors = descriptors
  )

  # Step 6: Build clean environment
  clean_env <- new.env(parent = baseenv())

  # Attach declared packages
  pkg_names <- if (is.list(packages)) names(packages) else packages
  for (pkg in pkg_names) {
    pkg_ns <- asNamespace(pkg)
    pkg_exports <- getNamespaceExports(pkg)
    for (export_name in pkg_exports) {
      clean_env[[export_name]] <- get(export_name, envir = pkg_ns)
    }
  }

  # Always make gofigR::publish available
  clean_env$publish <- gofigR::publish

  # Inject resolved parameter values
  for (nm in names(resolved_params)) {
    clean_env[[nm]] <- resolved_params[[nm]]
  }

  # Inject clean room context (discovered by publish() via parent.frame())
  clean_env$.gf_clean_room_context <- context

  # Step 7: Execute -- either interactive gadget or single-shot
  if (interactive) {
    run_reproducible_gadget(fn, descriptors, resolved_params, packages, clean_env, context)
  } else {
    eval(body(fn), envir = clean_env)
  }
}


# ---- Shiny gadget for interactive parameter exploration ----

#' Build a Shiny input widget from a gf_param descriptor.
#' @param ns Shiny namespace function
#' @param name parameter name
#' @param desc gf_param descriptor
#' @return Shiny input element, or NULL for static params
param_to_shiny_input <- function(ns, name, desc) {
  if (is.null(desc$widget)) return(NULL)  # static params have no widget

  input_id <- ns(name)
  label <- name

  switch(desc$widget,
    slider = {
      step <- desc$step %||% if (desc$type == "integer") 1L else 0.1
      shiny::sliderInput(input_id, label,
                         min = desc$min, max = desc$max,
                         step = step, value = desc$default)
    },
    dropdown = {
      shiny::selectInput(input_id, label,
                         choices = desc$choices, selected = desc$default)
    },
    checkbox = {
      shiny::checkboxInput(input_id, label, value = desc$default)
    },
    text = {
      shiny::textInput(input_id, label, value = desc$default)
    },
    NULL
  )
}

#' Run the Shiny gadget for interactive clean room exploration.
#' @param fn the user's function
#' @param descriptors named list of gf_param descriptors
#' @param resolved_params named list of default values
#' @param packages package names
#' @param clean_env the clean execution environment
#' @param context the clean room context
run_reproducible_gadget <- function(fn, descriptors, resolved_params,
                                    packages, clean_env, context) {
  # Identify interactive params (those with widgets)
  interactive_params <- Filter(function(d) !is.null(d$widget), descriptors)

  ui <- shiny::fluidPage(
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
             padding: 0; margin: 0; }
      .gadget-footer { padding: 10px 15px; border-top: 1px solid #e0e0e0; }
      .gadget-buttons { display: flex; justify-content: flex-end; gap: 8px; }
    "))),
    shiny::sidebarLayout(
      shiny::sidebarPanel(width = 3,
        shiny::h5(context$name %||% "Clean Room"),
        shiny::hr(),
        lapply(names(interactive_params), function(nm) {
          param_to_shiny_input(identity, nm, interactive_params[[nm]])
        })
      ),
      shiny::mainPanel(width = 9,
        shiny::plotOutput("plot", width = "100%", height = "500px"),
        # Publish status area (loader + watermark/link)
        shinyjs::hidden(shiny::div(id = "publish-loader",
          style = "padding: 10px; color: #888;",
          shiny::icon("spinner", class = "fa-spin"),
          "Publishing..."
        )),
        shiny::div(id = "publish-result",
          shiny::uiOutput("publish_status")
        )
      )
    ),
    shiny::div(class = "gadget-footer",
      shiny::div(class = "gadget-buttons",
        shiny::actionButton("publish", "Publish",
                            icon = shiny::icon("cloud-arrow-up"),
                            class = "btn-primary"),
        shiny::actionButton("done", "Done", class = "btn-default")
      )
    )
  )

  server <- function(input, output, session) {
    values <- shiny::reactiveValues(revision = NULL)

    # Build reactive param values from Shiny inputs + static defaults
    current_params <- shiny::reactive({
      params <- resolved_params
      for (nm in names(interactive_params)) {
        val <- input[[nm]]
        if (!is.null(val)) {
          # Preserve integer type for integer sliders
          if (interactive_params[[nm]]$type == "integer" && is.numeric(val)) {
            val <- as.integer(val)
          }
          params[[nm]] <- val
        }
      }
      params
    })

    # Helper: build a fresh execution environment with current params
    build_exec_env <- function(params) {
      exec_env <- new.env(parent = parent.env(clean_env))
      for (nm in ls(clean_env, all.names = TRUE)) {
        exec_env[[nm]] <- clean_env[[nm]]
      }
      for (nm in names(params)) {
        exec_env[[nm]] <- params[[nm]]
      }
      exec_env
    }

    # Re-execute function body on param change, capture plot
    output$plot <- shiny::renderPlot(res = 120, {
      params <- current_params()
      exec_env <- build_exec_env(params)

      # Replace publish() with a no-op that just prints the plot
      # (we don't want to publish on every param change)
      exec_env$publish <- function(plot_obj, ...) {
        print(plot_obj)
        invisible(plot_obj)
      }

      # Clear previous publish result when params change
      values$revision <- NULL

      eval(body(fn), envir = exec_env)
    })

    # Publish status: show QR code + link after publishing
    output$publish_status <- shiny::renderUI({
      rev <- values$revision
      if (is.null(rev)) return(NULL)

      url <- get_revision_url(rev)
      gfContainer(
        shiny::div(style = "margin: 10px;",
          shiny::tags$a(
            paste0("View on GoFigr: ",
                   rev$figure_metadata$name %||% context$name %||% "Figure",
                   " #", (rev$revision_index %||% 0) + 1),
            href = url, target = "_blank"
          )
        ),
        shiny::div(style = "margin: 10px;",
          shiny::HTML(get_qr_png(url))
        )
      )
    })

    # Publish button: execute with real publish()
    shiny::observeEvent(input$publish, {
      shinyjs::show("publish-loader")
      shinyjs::hide("publish-result")
      shinyjs::disable("publish")

      params <- current_params()
      exec_env <- build_exec_env(params)

      # Update context with current param values
      updated_descriptors <- context$descriptors
      for (nm in names(interactive_params)) {
        updated_descriptors[[nm]]$default <- params[[nm]]
      }
      exec_env$.gf_clean_room_context <- list(
        source = context$source,
        name = context$name,
        manifest = build_manifest(updated_descriptors, packages, list(), context$name),
        descriptors = updated_descriptors
      )

      # Capture the revision returned by publish()
      last_rev <- NULL
      exec_env$publish <- function(plot_obj, ...) {
        rev <- gofigR::publish(plot_obj, ...)
        last_rev <<- rev
        rev
      }

      tryCatch({
        eval(body(fn), envir = exec_env)
        values$revision <- last_rev
      }, error = function(e) {
        shiny::showNotification(paste("Publish failed:", e$message),
                                type = "error", duration = 5)
      })

      shinyjs::hide("publish-loader")
      shinyjs::show("publish-result")
      shinyjs::enable("publish")
    })

    # Done button
    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }

  viewer <- shiny::dialogViewer("Clean Room", width = 1100, height = 700)
  shiny::runGadget(ui, server, viewer = viewer, stopOnCancel = TRUE)
}
