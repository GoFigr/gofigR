#' Fetches a figure given an API ID.
#'
#' @param gf GoFigr client
#' @param api_id API ID for the figure
#'
#' @return figure object
#' @export
get.figure <- function(gf, api_id) {
  response.to.JSON(gofigr.GET(gf, paste0("figure/", api_id)))
}


#' Creates a new figure. The created figure will be blank and won't contain
#' any revisions.
#'
#' @param gf GoFigr object
#' @param analysis analysis under which to create the figure
#' @param name name for the new figure
#' @param description description for the new figure
#'
#' @return created figure object
#' @export
create.figure <- function(gf, analysis, name, description=NULL) {
  response.to.JSON(gofigr.POST(gf, "figure/",
                               body=obj.to.JSON(list(name=name,
                                                     description=null.to.empty(description),
                                                     analysis=get.api.id(analysis))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

#' Finds a figure by name
#'
#' @param gf GoFigr client
#' @param analysis parent analysis
#' @param name name of the figure to find
#' @param description description of the figure if it needs to be created
#' @param create if TRUE and the figure doesn't exist, it will be created; throws an error otherwise.
#'
#' @return figure object
#' @export
find.figure <- function(gf, analysis, name, description=NULL, create=FALSE) {
  ana <- get.analysis(gf, get.api.id(analysis))
  find.or.create(gf, name, create=create,
                 type="figure",
                 get.list=function() { ana$figures },
                 do.create=function() {
                   create.figure(gf, name, description, analysis=get.api.id(ana))
                 })
}
