runtime_options <- structure(local({
  intercept_enabled=FALSE
  gofigr_options=NULL
  environment()
}))

is_intercept_on <- function() {
  return(runtime_options$intercept_enabled)
}

intercept_on <- function() {
  runtime_options$intercept_enabled <- TRUE
}

intercept_off <- function() {
  runtime_options$intercept_enabled <- FALSE
}

get_rstudio_file_path <- function() {
  tryCatch({
    if(interactive() && rstudioapi::isAvailable()) {
      return(rstudioapi::getSourceEditorContext()$path)
    } else {
      return(NULL)
    }
  }, error=function(err) {
    return(NULL)
  })
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

check_configured <- function(response=warning) {
  if(is.null(get_options())) {
    response("GoFigr not configured. Did you call enable()?")
    return(FALSE)
  }
  return(TRUE)
}

#' Captures output of an expression and publishes it to GoFigr. This function
#' is helpful if you have multiple function calls which incrementally build a
#' single figure.
#'
#' @param expr expression which generates your figure
#' @param name figure name. Use NULL to infer automatically
#' @param data input data to publish with the figure
#' @param env environment in which to evaluate the captured expression (default=parent.frame())
#'
#' @return result of evaluating your expression
#' @export
capture <- function(expr,
                    name=NULL,
                    data=NULL,
                    env=parent.frame()) {
  # Convert to quosure to capture the calling environment
  quos <- rlang::as_quosure(substitute(expr), env)

  # GoFigr assumes that the first argument is the plot object, to be published
  # as an RDS file. Hence the "pointless" function(data) wrapper.
  wrapper <- intercept(function() {
      res <- eval(rlang::get_expr(quos),
                  envir = rlang::get_env(quos))
      if(!is.null(res) && is_supported(res)) {
        gf_print(res) # Implicitly plot the return value
      }
  }, force=TRUE, data=data, figure_name=name)
  invisible(wrapper())
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

infer_input_path <- function() {
  if(interactive()) {
    return(rstudioapi::getSourceEditorContext()$path)
  } else {
    return(knitr::current_input(TRUE))
  }
}

is_debug_on <- function() {
  opts <- get_options()
  if(is.null(opts)) {
    return(FALSE)
  } else {
    return(opts$debug)
  }
}

split_args <- function(...) {
  args <- list(...)

  if(length(args) == 0) {
    return(NULL)
  }

  first <- args[[1]]
  rest <- args[-1]

  if(length(args) == 1) {
    other_args <- list()
  } else if(length(args) >= 2) {
    other_args <- rest
  } else {
    other_args <- list()
  }
  return(list(first=first, rest=other_args))
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
  } else if(is(p, "ggplot")) {
    return(p$labels$title)
  } else {
    return(NULL)
  }
}

print_revision <- function(rev, ...) {
  message(paste0(get_revision_url(rev), "\n"))
}

get_interactive_context <- function() {
  histfile <- tempfile()
  utils::savehistory(histfile)

  return(list(input_path=histfile,
              chunk_code=NULL,
              metadata=list(context="interactive")))
}

get_execution_context <- function() {
  if(!is.null(knitr::current_input())) {
    # Running in knitr
    options <- knitr::opts_current$get()
    return(list(input_path=knitr::current_input(),
                chunk_code=paste0(options$code, collapse="\n"),
                metadata=list(context='knitr',
                              knitr_options=options)))
  } else if(!interactive()) {
    # Running in a script
    return(list(input_path=scriptName::current_filename(),
                chunk_code=NULL,
                metadata=list(context='script')))
  } else if(interactive() && rstudioapi::isAvailable()) {
    # Running interactively in RStudio
    list(input_path=rstudioapi::getSourceEditorContext()$path,
         chunk_code=NULL,
         metadata=list(context='RStudio'))
  } else if(interactive() && !rstudioapi::isAvailable()) {
    # Running interactive outside of RStudio
    return(get_interactive_context())
    } else {
      warning("GoFigr could not detect the execution context. Please contact support@gofigr.io.")
      return(list())
  }
}

#' Wraps a plotting function (e.g. heatmap.2) so that its output is intercepted
#' by GoFigr.
#'
#' @param plot_func function to intercept
#' @param supported_classes calls will be intercepted *only if* the first argument is an \
#' instance of any of these classes
#' @param force force intercept even if auto_publish is off
#' @param data data to publish with the figure
#' @param figure_name name of the figure, if different from default
#'
#' @return intercepted function
#' @export
#'
#' @examples
#' heatmap.2 <- intercept(gplots::heatmap.2)
intercept <- function(plot_func,
                      supported_classes=NULL, force=FALSE,
                      data=NULL, figure_name=NULL) {
  # Make sure we don't capture anything internally called by plot_func
  base_func <- suppress(plot_func)

  function(...) {
    if(!is_intercept_on() && !force) {
      return(base_func(...))
    } else if(!check_configured()) {
      return(base_func(...))
    }

    tryCatch({
      # Check if the first argument is a plot we support. If not, defer to base_func
      if(!is.null(supported_classes) && !any(class(split_args(...)$first) %in% supported_classes)) {
        return(base_func(...))
      }

      plot_obj <- split_args(...)$first

      context <- get_execution_context()

      rev <- publish(plot_obj, figure_name=figure_name,
              input_path=context$input_path,
              chunk_code=context$chunk_code,
              args=list(...),
              base_func=base_func,
              data=data,
              metadata=context$metadata)

      gf_opts <- get_options()
      show <- gf_opts$show

      if(is_debug_on()) {
        message(paste0("Preparing to display figure. Revision: ",
                       if(!is.null(rev)) rev$api_id else "NULL",
                       ". Show: ", show,
                       ". Watermark not null: ", !is.null(gf_opts$watermark)))
      }

      if(!is.null(rev) && show == "watermark" && !is.null(gf_opts$watermark)) {
        if(is_debug_on()) {
          message("Showing watermark")
        }

        qr <- gf_opts$watermark(rev, NULL)

        base::plot(ggwatermark(qr, plot_obj, function() {base_func(...)}))
      } else {
        if(is_debug_on()) {
          message("Showing original")
        }

        base_func(...)
      }
    }, error=function(err) {
      warning(paste0("Error when publishing figure, will show original instead: ", err))
      base_func(...)
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
                         `Sys.info()`=as.list(Sys.info()))
  for(name in names(metadata)) {
    if("knitr_strict_list" %in% class(metadata[[name]])) {
      total_metadata[[name]] <- do.call(list, metadata[[name]])
    } else {
      total_metadata[[name]] <- metadata[[name]]
    }
  }

  rev <-  gofigR::create_revision(client, fig,
                                  metadata = total_metadata)

  return(rev)
}


apply_watermark <- function(rev_bare, plot_obj, args, base_func, gf_opts) {
  if(!is.null(gf_opts$watermark)) {
    watermarked_path <- tempfile(fileext=".png")
    qr <- gf_opts$watermark(rev_bare, NULL)
    p <- ggwatermark(qr, plot_obj, function() {
        do.call(base_func, args)
      })
    suppressMessages(ggsave(watermarked_path, plot=p))

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
                     source_path, chunk_code=NULL,
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
  return(data)
}


save_as_image_file <- function(format, plot_obj, args, base_func, options) {
  path <- tempfile(fileext=paste0(".", format))
  do_plot <- function() { do.call(base_func, args) }

  p <- cowplot::ggdraw()

  if(is_ggplot(plot_obj)) {
    p <- p + cowplot::draw_plot(plot_obj)
  } else {
    p <- p + cowplot::draw_plot(do_plot)
  }

  suppressMessages(ggsave(path, plot=p))

  return(path)
}

save_as_image <- function(format, plot_obj, args, base_func, options) {
  path <- save_as_image_file(format, plot_obj, args, base_func, options)
  img_obj <- make_image_data("figure", path, format, FALSE)
  file.remove(path)
  return(img_obj)
}

check_show_setting <- function(show) {
  if(is.null(show) || !show %in% list("watermark", "original")) {
    stop(paste0("Valid show values are watermark or original, but you passed: ", show))
  }
}

publish <- function(plot_obj, figure_name,
                    input_path=NULL, chunk_code=NULL, image_formats=c("eps"),
                    args=list(), base_func=base::plot,
                    data=NULL, metadata=list()) {

  gf_opts <- get_options()
  if(is.null(gf_opts)) {
    warning("GoFigr hasn't been configured. Did you call gofigR::enable()?")
    return(invisible(NULL))
  }

  if(is_debug_on()) {
    message(paste0("Starting publish. Devices: ", paste0(names(grDevices::dev.list()), collapse=", ")))
  }

  client <- gf_opts$client
  if(is.null(figure_name)) {
    figure_name <- get_title(plot_obj)
  }

  if(is.null(figure_name)) {
    figure_name <- "Anonymous Figure"
    warning("Your figure lacks a name and will be published as Anonymous Figure.")
  }

  png_path <- save_as_image_file("png", plot_obj, args, base_func, options)
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
  watermark_data <- apply_watermark(rev_bare, plot_obj, args, base_func, gf_opts)
  if(!is.null(watermark_data)) {
    image_data <- append(image_data, watermark_data$data_object)
  }

  # Additional image formats
  image_data <- append(image_data, lapply(image_formats, function(fmt) {
    save_as_image(fmt, plot_obj, args, base_func, options)
  }))
  image_data <- image_data[!is.null(image_data)]

  other_data <- annotate(rev_bare, plot_obj, fig$name, input_path, chunk_code, data)

  rev <- gofigR::update_revision_data(client, rev_bare, silent=TRUE, new_data=append(image_data, other_data))
  file.remove(png_path)

  if(gf_opts$verbose) {
    message(paste0("\"", fig$name, "\" at ", get_revision_url(rev), "\n"))
  }

  if(is_debug_on()) {
    message(paste0("Ending publish. Devices: ", paste0(names(grDevices::dev.list()), collapse=", ")))
  }

  return(rev)
}

get_supported_classes <- function() {
  return(c("ggplot"))
}

is_supported <- function(x) {
  return(any(class(x) %in% get_supported_classes()))
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
gf_plot <- make_invisible(intercept(base::plot, supported_classes=get_supported_classes()))

#' Prints and publishes an object (if supported)
#' @param ... passed directly to print
#' @returns result of the call to print(...)
#'
#' @export
gf_print <- make_invisible(intercept(base::print, supported_classes=get_supported_classes()))

intercept_base <- function(env=.GlobalEnv) {
  assign("plot", gf_plot, env)
  assign("print", gf_print, env)
}

#' Enables GoFigr support.
#'
#' @param analysis_api_id Analysis API ID (if analysis_name is NULL)
#' @param analysis_name Analysis name (if analysis_api_id is NULL)
#' @param workspace API ID of the workspace
#' @param create_analysis if TRUE and analysis_name does not exist, it will be automatically created
#' @param analysis_description analysis description if creating a new analysis
#' @param watermark watermark class to use, e.g. QR_WATERMARK, LINK_WATERMARK or NO_WATERMARK
#' @param auto_publish will publish all plots automatically if TRUE. Note
#'  that setting this option will re-assign plot() and print() in the global environment.
#' @param verbose whether to show verbose output
#' @param debug whether to show debugging information
#' @param show which figure to display in the document: original, watermark, or hide. Note that this setting \
#' only affects the display and doesn't change what gets published: e.g. even if you choose to display \
#' the original figure, the watermarked version will still be published to GoFigr.
#'
#' @return named list of GoFigr options
#' @export
enable <- function(analysis_api_id=NULL,
                   analysis_name=NULL,
                   workspace=NULL,
                   create_analysis=TRUE,
                   analysis_description=NULL,
                   watermark=QR_WATERMARK,
                   auto_publish=TRUE,
                   verbose=FALSE,
                   debug=FALSE,
                   show="watermark") {
  check_show_setting(show)

  context <- get_execution_context()
  if(is.null(analysis_name) && !is.null(context$input_path)) {
    analysis_name <- basename(context$input_path)
    message(paste0("Analysis name: ", analysis_name))
  }

  # Create the GoFigr client
  gf <- gofigr_client(workspace=workspace, verbose=verbose)

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

    return(environment())
  })))

  intercept_on()
  if(auto_publish) {
    intercept_base()
  }

  return(invisible(get_options()))
}
