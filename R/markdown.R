#' Implements the GoFigr hook for knitr's fig.process.
#'
#' @param path figure path
#' @param options chunk options
#'
#' @return updated figure path
#' @export
gofigr.fig.process <- function(path, options) {
  print(path)

  gf_opts <- options$gofigr
  if(is.null(gf_opts)) {
    warn("GoFigr hasn't been configured. Did you call gofigR::enable.knitr?")
    return()
  }

  figure_name <- options$gofigr.figure.name
  if(is.null(figure_name)) {
    figure_name <- options$label
  }

  if(is.null(figure_name)) {
    figure_name <- "Anonymous Figure"
    warn("Your figure lacks a name and will be published as Anonymous Figure.")
  }

  image_format <- tolower(tools::file_ext(path))
  extra_image_formats <- list()
  if(image_format == "pdf") {
    tmp_image_path <- tempfile(fileext = ".png")
    animation::im.convert(path, tmp_image_path, extra.opts=options$gofigr.im.options,
                          convert = "convert")
    if(file.exists(tmp_image_path)) {
      extra_image_formats <- force(list(make.image.data("figure", tmp_image_path, "png", TRUE)))

      file.remove(tmp_image_path)
    }
  }

  figure_name <- paste0(figure_name, " - ", default.if.null(options$fig.num, "NA"))

  sess <- sessionInfo()
  info <- capture.output({print(sess)})

  gf <- gf_opts$client
  fig <- gofigR::find.figure(gf, gf_opts$analysis, figure_name, create=TRUE)

  # Create a bare revision to get API ID
  rev.bare <- gofigR::create.revision(gf, fig,
                                      metadata = list(input=knitr::current_input(),
                                                      `getwd`=getwd(),
                                                      `R version`=paste0(info[[1]]),
                                                      `R details`=sess$R.version))

  rev <- gofigR::update.revision.data(gf, rev.bare, silent=TRUE, data=append(list(
    make.code.data("Current chunk", options$code, "R"),
    make.code.data("Complete Markdown input", file(knitr::current_input()), "Rmd"),
    make.image.data("figure", path, image_format, TRUE),
    make.image.data("figure", path, image_format, FALSE),

    make.text.data("sessionInfo", paste0(info, collapse="\n"))
  ), extra_image_formats))

  return(path)
}

#' Enables GoFigr within knitr.
#'
#' @param analysis.api.id Analysis API ID (if not using analysis.name)
#' @param analysis.name Analysis name (if not using analysis.api.id)
#' @param workspace parent workspace
#' @param create.analysis if TRUE and the analysis name does not exist, it will be created
#' @param analysis.description description for the analysis if it needs to be created
#'
#' @return GoFigr client
#' @export
enable.knitr <- function(analysis.api.id=NULL,
                         analysis.name=NULL,
                         workspace=NULL,
                         create.analysis=TRUE,
                         analysis.description=NULL,
                         im.options="-density 300") {
  # Create the GoFigr client
  gf <- gofigr.client(workspace=workspace)

  # Find the analysis
  if(!is.null(analysis.api.id)) {
    ana <- gofigR::get.analysis(analysis.api.id)
  } else if(!is.null(analysis.name)) {
    ana <- gofigR::find.analysis(gf, analysis.name,
                                 workspace = gf$workspace,
                                 create = create.analysis,
                                 description = analysis.description)
  } else {
    stop("Please specify either analysis.api.id or analysis.name")
  }

  knitr::opts_chunk$set(
    gofigr=list(client=gf,
                analysis=ana,
                workspace=gf$workspace),
    fig.process=gofigr.fig.process,
    gofigr.im.options=im.options
  )

  return(gf)
}
