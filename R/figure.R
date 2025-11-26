#' Fetch a single figure by API ID.
#'
#' This is a low-level helper that retrieves the raw figure object from the
#' GoFigr API. It is typically used after you already know the figure's
#' `api_id`, for example from an analysis or figure listing.
#'
#' @param gf GoFigr client created by `gofigr_client()`.
#' @param api_id Character string with the API ID of the figure to fetch.
#'
#' @return A figure object as returned by the API (an environment), including
#'   metadata and revision information.
#' @export
get_figure <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("figure/", api_id)))
}


#' Create a new figure under an analysis.
#'
#' The newly created figure is initially blank and contains no revisions. Use
#' the revision helpers to attach content once the figure has been created.
#'
#' @param gf GoFigr client.
#' @param analysis Parent analysis under which to create the figure. Can be
#'   an analysis object or an API ID.
#' @param name Human-readable name for the new figure.
#' @param description Optional longer description of the figure's purpose or
#'   contents.
#'
#' @return The created figure object as returned by the API.
#' @export
create_figure <- function(gf, analysis, name, description=NULL) {
  response_to_JSON(gofigr_POST(gf, "figure/",
                               body=obj_to_JSON(list(name=name,
                                                     description=null_to_empty(description),
                                                     analysis=get_api_id(analysis))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

#' Find a figure by name within an analysis.
#'
#' Searches the figures attached to a given analysis by name and optionally
#' creates a new figure when no match is found. This is often the most
#' convenient way to obtain a figure handle in scripts and notebooks.
#'
#' @param gf GoFigr client.
#' @param analysis Parent analysis object (or environment) in which to look
#'   for the figure.
#' @param name Name of the figure to find.
#' @param description Optional description to use if a new figure must be
#'   created.
#' @param create Logical; if `TRUE` and the figure doesn't exist, a new one
#'   is created. If `FALSE`, an error is thrown when no matching figure is
#'   found.
#'
#' @return A figure object corresponding to the matching (or newly created)
#'   figure.
#' @export
find_figure <- function(gf, analysis, name, description=NULL, create=FALSE) {
  if(is.null(analysis)) {
    stop("Please specify an analysis")
  } else if(is.character(analysis)) {
    stop("Please pass a valid analysis object instead of just the ID")
  }

  if(is.null(analysis$figures)) {
    analysis$figures <- get_analysis(gf, analysis$api_id)$figures
  }

  find_or_create(gf, name, create=create,
                 type="figure",
                 get_list=function() { analysis$figures },
                 do_create=function() {
                   new_fig <- create_figure(gf, name, description, analysis=analysis)
                   analysis$figures <<- append(analysis$figures, list(new_fig))
                   return(new_fig)
                 })
}
