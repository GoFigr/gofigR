set_options <- function(options) {
  assign("gofigr_options", options, .GlobalEnv)
}

get_options <- function() {
  return(.GlobalEnv[["gofigr_options"]])
}

infer_input_path <- function() {
  if(interactive()) {
    return(rstudioapi::getSourceEditorContext()$path)
  } else {
    return(knitr::current_input(TRUE))
  }
}

rstudio_chunk_callback <- function(chunkName, chunkCode) {
  tryCatch({
    chunkCode <- textutils::HTMLdecode(chunkCode)
    lapply(.GlobalEnv$rstudio_deferred_plots, function(defplot) {
      defplot(chunkName, chunkCode)
    })
  }, finally=function() {
    assign("rstudio_deferred_plots", list(), .GlobalEnv)
  })
  return(list())
}

split_args <- function(...) {
  args <- list(...)

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

#' Replacement for plot() in a knitr context. Captures the plot and publishes
#' it to GoFigr.
#'
#' @param ... same as plot()
#'
#' @return same as plot()
#' @export
plot_knitr <- function(..., base_func) {
  options <- knitr::opts_current

  # Is GoFigr disabled for current chunk?
  if(identical(options$gofigr_on, FALSE)) {
    return(base_func(...))
  }

  args <- split_args(...)

  figure_name <- options$gofigr_figure_name
  if(is.null(figure_name)) {
    figure_name <- options$get("label")
  }

  if(is.null(figure_name)) {
    figure_name <- "Anonymous Figure"
    warning("Your figure lacks a name and will be published as Anonymous Figure.")
  }

  publish(args$first, figure_name=figure_name, show=TRUE,
          input_path=knitr::current_input(),
          chunk_code=paste0(options$get("code"), collapse="\n"),
          other_args=args$rest)
}

parse_params <- function(chunk, patterns=all_patterns$md) {
  header <- stringr::str_split(chunk, "\n")[[1]]
  m <- stringr::str_match(header, patterns$chunk.begin)
  m <- m[!is.na(m[, 1]), , drop=FALSE]
  if(length(m) == 0) {
    return(list()) # no params
  } else {
    return(xfun::csv_options(m[1, 3]))
  }
}

#' Replacement for plot within RStudio. Captures the plot and publishes to GoFigr.
#'
#' @param ... same as plot()
#'
#' @return same as plot()
#' @export
plot_rstudio <- function(..., base_func) {
  base_func(...)

  args <- split_args(...)
  plot_obj = args$first

  defplots <<- append(.GlobalEnv$rstudio_deferred_plots, function(chunkName, chunkCode) {
    options <- parse_params(chunkCode)

    # Is GoFigr disabled for current chunk?
    if(identical(options$gofigr_on, FALSE)) {
      return(base_func(...))
    }

    figure_name <- options$gofigr_figure_name
    if(is.null(figure_name)) {
      figure_name <- chunkName
    }

    if(is.null(figure_name)) {
      figure_name <- "Anonymous Figure"
      warning("Your figure lacks a name and will be published as Anonymous Figure.")
    }

    publish(args$first, figure_name=figure_name, show=FALSE,
            input_path=rstudioapi::getSourceEditorContext()$path,
            chunk_code=chunkCode,
            other_args=args$rest,
            base_func=base_func)
    })

  assign("rstudio_deferred_plots", defplots, .GlobalEnv)
}


#' Replacement for plot in an interactive session outside of RStudio.
#' Captures the plot and publishes to GoFigr.
#'
#' @param ... same as plot()
#'
#' @return same as plot()
#' @export
plot_interactive <- function(..., base_func) {
  args <- split_args(...)

  histfile <- tempfile()
  savehistory(histfile)

  on.exit({ file.remove(histfile) })
  publish(args$first, figure_name="Anonymous Figure", show=TRUE,
          input_path=histfile,
          chunk_code=NULL,
          other_args=args$rest,
          base_func=base_func)
}

#' Replacement for plot in a script. Captures the plot and publishes to GoFigr.
#'
#' @param ... same as plot()
#'
#' @return same as plot()
#' @export
plot_script <- function(..., base_func) {
  args <- split_args(...)

  publish(args$first, figure_name="Anonymous Figure", show=TRUE,
          input_path=scriptName::current_filename(),
          chunk_code=NULL,
          other_args=args$rest,
          base_func=base_func)
}

make_gofigr_intercept <- function(base_func, supported_classes=NULL) {
  function(...) {
    if(!is.null(supported_classes) && !any(class(split_args(...)$first) %in% supported_classes)) {
      return(base_func(...))
    }

    if(!is.null(knitr::current_input())) {
      # Running in knitr
      plot_knitr(..., base_func=base_func)
    } else if(!interactive()) {
      # Running in a script
      plot_script(..., base_func=base_func)
    } else if(interactive() && rstudioapi::isAvailable()) {
      # Running interactively in RStudio
      plot_rstudio(..., base_func=base_func)
    } else if(interactive() && !rstudioapi::isAvailable()) {
      # Running interactive outside of RStudio
      plot_interactive(..., base_func=base_func)
    } else {
      warning("GoFigr could not detect the execution context. Please contact support@gofigr.io.")
      base_func(...)
    }
  }
}

create_bare_revision <- function(client, fig, input_path) {
  sess <- sessionInfo()
  info <- capture.output({base::print(sess)})

  rev <-  gofigR::create_revision(client, fig,
                                  metadata = list(input=input_path,
                                                  `getwd`=getwd(),
                                                  `R version`=paste0(info[[1]]),
                                                  `R details`=sess$R.version,
                                                  `Sys.info()`=as.list(Sys.info())))

  return(rev)
}


apply_watermark <- function(rev_bare, png_path, gf_opts) {
  if(!is.null(gf_opts$watermark) && !is.null(png_path)) {
    primary_img <- magick::image_read(png_path)
    watermarked <- gf_opts$watermark(rev_bare, primary_img)
    watermarked_path <- paste0(png_path, "_watermarked.png")
    magick::image_write(watermarked, path=watermarked_path)

    image_destroy(primary_img)
    image_destroy(watermarked)
    return(list(make_image_data("figure", watermarked_path, "png", TRUE)))
  } else {
    return(list())
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
                     source_path, chunk_code=NULL) {
  sess <- sessionInfo()
  info <- capture.output({base::print(sess)})

  # Plot object as RDS file
  data <- list(capture_rds(plot_obj,
                           paste0(default_if_null(figure_name, "plot_object"), ".RDS")),
               make_text_data("sessionInfo", paste0(info, collapse="\n")))

  if(!is.null(chunk_code)) {
    data <- append(data, list(make_code_data("Current chunk", chunk_code, "R")))
  }

  if(!is.null(source_path)) {
    data <- append(data, list(make_code_data("Input file", file(source_path),
                                             tools::file_ext(source_path))))
  }
  return(data)
}


save_as_image <- function(format, plot_obj, other_args, base_func) {
  path <- tempfile(fileext=paste0(".", format))
  if(format == "png") {
    png(path)
  } else if(format == "svg") {
    svg(path)
  } else if(format == "pdf") {
    pdf(path)
  } else if(format == "eps") {
    setEPS()
    postscript(path)
  } else {
    warning(paste0("Unsupported image format: ", format))
    return(NULL)
  }

  tryCatch({
    do.call(base_func, append(list(plot_obj), other_args))
    dev.off()
    return(make_image_data("figure", path, format, FALSE))
  }, finally={
    if(file.exists(path)) {
      file.remove(path)
    }
  })
}


publish <- function(plot_obj, figure_name, show=TRUE,
                    input_path=NULL, chunk_code=NULL, image_formats=c("svg", "eps"),
                    other_args=list(), base_func=base::plot) {
  show_plot <- function() {
    if(show) {
      do.call(base_func, append(list(plot_obj), other_args))
    } else {
      return(NULL)
    }
  }

  gf_opts <- get_options()
  if(is.null(gf_opts)) {
    warning("GoFigr hasn't been configured. Did you call gofigR::enable()?")
    return(show_plot())
  }

  if(identical(gf_opts$gofigr_on, FALSE)) {
    return(do.call(base_func, other_args))
  }

  client <- gf_opts$client

  png_path <- tempfile(fileext=".png")
  png(png_path)
  tryCatch({
    do.call(base_func, append(list(plot_obj), other_args))
  }, finally={
  dev.off()
  })

  fig <- gofigR::find_figure(gf_opts$client, gf_opts$analysis, figure_name, create=TRUE)

  # Create a bare revision to get API ID
  rev_bare <- create_bare_revision(client, fig, input_path)

  # Now that we have an API ID, apply the watermark
  image_data <- list(make_image_data("figure", png_path, "png", FALSE))
  image_data <- append(image_data, apply_watermark(rev_bare, png_path, gf_opts))

  # Additional image formats
  image_data <- append(image_data, lapply(image_formats, function(fmt) {
    save_as_image(fmt, plot_obj, other_args, base_func)
  }))
  image_data <- image_data[!is.null(image_data)]

  other_data <- annotate(rev_bare, plot_obj, fig$name, input_path, chunk_code)

  rev <- gofigR::update_revision_data(client, rev_bare, silent=TRUE, new_data=append(image_data, other_data))
  file.remove(png_path)

  if(identical(gf_opts$verbose, TRUE)) {
    cat(paste0(fig$name, " - ", rev$api_id, "\n"))
  }

  show_plot()
}


#' Enables GoFigr support.
#'
#' @param analysis_api_id Analysis API ID (if analysis_name is NULL)
#' @param analysis_name Analysis name (if analysis_api_id is NULL)
#' @param workspace API ID of the workspace
#' @param create_analysis if TRUE and analysis_name does not exist, it will be automatically created
#' @param analysis_description analysis description if creating a new analysis
#' @param watermark watermark class to use, e.g. QR_WATERMARK, LINK_WATERMARK or NO_WATERMARK
#' @param auto_publish will publish all plots automatically if TRUE
#' @param verbose whether to show verbose output
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
                   verbose=FALSE) {
  # Create the GoFigr client
  gf <- gofigr_client(workspace=workspace)

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

  old_opts <- get_options()
  if(!is.null(old_opts) && !is.null(old_opts$rstudio_callback)) {
    rstudioapi::unregisterChunkCallback(old_opts$rstudio_callback)
  }

  assign("rstudio_deferred_plots", list(), .GlobalEnv)

  set_options(list(client=gf,
                   analysis=ana,
                   workspace=gf$workspace,
                   watermark=watermark,
                   verbose=verbose,
                   rstudio_callback=tryCatch( # only works in RStudio
                     {rstudioapi::registerChunkCallback(rstudio_chunk_callback)},
                     error=function(err) {NULL})))

  if(auto_publish) {
    assign("plot", make_gofigr_intercept(base::plot), .GlobalEnv)
    assign("print", make_gofigr_intercept(base::print, supported_classes=c("ggplot")), .GlobalEnv)
  }

  return(invisible(get_options()))
}
