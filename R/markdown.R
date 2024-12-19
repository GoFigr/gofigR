gofigr_fig_process_internal <- function(path, options) {
  if(identical(options$gofigr_on, FALSE)) {
    return(path)
  }

  gf_opts <- options$gofigr
  if(is.null(gf_opts)) {
    warn("GoFigr hasn't been configured. Did you call gofigR::enable_knitr?")
    return()
  }

  figure_name <- options$gofigr_figure_name
  if(is.null(figure_name)) {
    figure_name <- options$label
  }

  if(is.null(figure_name)) {
    figure_name <- "Anonymous Figure"
    warn("Your figure lacks a name and will be published as Anonymous Figure.")
  }

  image_format <- tolower(tools::file_ext(path))
  extra_image_formats <- list()

  primary_image_path <- NULL # will be used for the watermark
  if(image_format == "pdf") {
    tmp_image_path <- tempfile(fileext = ".png")
    animation::im.convert(path, tmp_image_path, extra.opts=options$gofigr_im_options,
                          convert = "convert")
    if(file.exists(tmp_image_path)) {
      extra_image_formats <- force(list(make_image_data("figure", tmp_image_path, "png", FALSE)))

      primary_image_path <- tmp_image_path
    }
  } else {
    primary_image_path <- path
  }

  figure_name <- paste0(figure_name, " - ", default_if_null(options$fig.num, "NA"))

  sess <- sessionInfo()
  info <- capture.output({print(sess)})

  gf <- gf_opts$client
  fig <- gofigR::find_figure(gf, gf_opts$analysis, figure_name, create=TRUE)

  # Create a bare revision to get API ID
  rev_bare <- gofigR::create_revision(gf, fig,
                                      metadata = list(input=knitr::current_input(),
                                                      `getwd`=getwd(),
                                                      `R version`=paste0(info[[1]]),
                                                      `R details`=sess$R.version,
                                                      `Sys.info()`=as.list(Sys.info())))

  # Now that we have an API ID, apply the watermark
  watermarked_path <- NULL
  if(!is.null(options$gofigr_watermark) && !is.null(primary_image_path)) {
    primary_img <- magick::image_read(primary_image_path)
    watermarked <- options$gofigr_watermark(rev_bare, primary_img)
    watermarked_path <- paste0(path, "_watermarked.png")
    magick::image_write(watermarked, path=watermarked_path)

    image_destroy(primary_img)
    image_destroy(watermarked)

    extra_image_formats <- append(extra_image_formats,
                                  list(make_image_data("figure", watermarked_path, "png", TRUE)))
  }

  rev <- gofigR::update_revision_data(gf, rev_bare, silent=TRUE, new_data=append(list(
    make_code_data("Current chunk", paste0(options$code, collapse="\n"), "R"),
    make_code_data("Complete Markdown input", file(knitr::current_input()), "Rmd"),
    make_image_data("figure", path, image_format, FALSE),

    make_text_data("sessionInfo", paste0(info, collapse="\n"))
  ), extra_image_formats))

  if(image_format == "pdf") { return(path) } # TODO: output PDF watermarks
  else if(!is.null(watermarked_path)) { return(watermarked_path) }
  else { return(path) }
}

#' Implements the GoFigr hook for knitr's fig.process.
#'
#' @param path figure path
#' @param options chunk options
#'
#' @return updated figure path
#' @export
gofigr_fig_process <- function(path, options) {
  tryCatch({gofigr_fig_process_internal(path, options)},
           error=function(e) {
             write(paste0("GoFigr publication failed: ", e, "\n"), stderr())
           },
           finally={
             gc() # image_destroy doesn't actually clear the on-disk cache until it's GC'd
           })
}

#' Enables GoFigr within knitr.
#'
#' @param analysis_api_id Analysis API ID (if not using analysis_name)
#' @param analysis_name Analysis name (if not using analysis_api_id)
#' @param workspace parent workspace
#' @param create_analysis if TRUE and the analysis name does not exist, it will be created
#' @param analysis_description description for the analysis if it needs to be created
#' @param watermark type of watermark to use. See watermark_generator() to customize. NULL for no watermark.
#'
#' @return GoFigr client
#' @export
enable_knitr <- function(analysis_api_id=NULL,
                         analysis_name=NULL,
                         workspace=NULL,
                         create_analysis=TRUE,
                         analysis_description=NULL,
                         im_options="-density 300",
                         watermark=QR_WATERMARK) {
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

  knitr::opts_chunk$set(
    gofigr=list(client=gf,
                analysis=ana,
                workspace=gf$workspace),
    fig.process=gofigr_fig_process,
    gofigr_im_options=im_options,
    gofigr_watermark=watermark
  )

  # Configure ImageMagick
  Sys.setenv(MAGICK_TEMPORARY_PATH=".")

  return(gf)
}
