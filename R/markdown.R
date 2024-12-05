#' Implements the GoFigr hook for knitr's fig.process.
#'
#' @param path figure path
#' @param options chunk options
#'
#' @return updated figure path
#' @export
gofigr.fig.process <- function(path, options) {
  print(path)
  print(options$gofigr)
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
                         analysis.description=NULL) {
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
    fig.process=gofigr.fig.process
  )

  return(gf)
}
