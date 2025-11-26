#' List analyses within a workspace.
#'
#' This is a convenience wrapper around `get_workspace()` that returns only
#' the analyses associated with a workspace, rather than the full workspace
#' object.
#'
#' @param gf GoFigr client.
#' @param workspace_id Optional API ID of the workspace to inspect. If `NULL`,
#'   the default workspace configured on the client (`gf$workspace`) is used.
#'
#' @return A list of analysis objects associated with the selected workspace.
#' @export
list_analyses <- function(gf, workspace_id=NULL) {
  worx <- get_workspace(gf, default_if_null(workspace_id, gf$workspace))
  return(worx$analyses)
}

#' Create a new analysis within a workspace.
#'
#' Analyses act as containers for figures, data and revisions. This helper
#' creates a fresh analysis under the specified workspace.
#'
#' @param gf GoFigr client.
#' @param name Human-readable analysis name.
#' @param description Optional longer description of the analysis.
#' @param workspace Workspace under which the analysis will be created. Can be
#'   a workspace object or an API ID. If `NULL`, the client's default workspace
#'   is used.
#'
#' @return The created analysis object as returned by the API.
#' @export
create_analysis <- function(gf, name, description=NULL, workspace=NULL) {
  response_to_JSON(gofigr_POST(gf, "analysis/",
                               body=obj_to_JSON(list(name=name,
                                                     description=null_to_empty(description),
                                                     workspace=infer_workspace(gf, workspace))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Fetch an analysis by API ID.
#'
#' @param gf GoFigr client.
#' @param api_id Character string with the API ID of the analysis to fetch.
#'
#' @return An analysis object as returned by the API.
#' @export
get_analysis <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("analysis/", api_id, "/")))
}


#' Delete an analysis by API ID.
#'
#' This permanently removes the analysis and its associated figures and
#' revisions from the workspace.
#'
#' @param gf GoFigr client.
#' @param api_id Character string with the API ID of the analysis to delete.
#'
#' @return Invisibly returns `NULL`. An error is thrown if the deletion fails.
#' @export
delete_analysis <- function(gf, api_id) {
  gofigr_DELETE(gf, paste0("analysis/", api_id),
                expected_status_code = 204)
}


#' Find an analysis by name, optionally creating it.
#'
#' Searches the analyses within a workspace by name and, optionally, creates a
#' new analysis when no match is found.
#'
#' @param gf GoFigr client.
#' @param name Name of the analysis to find.
#' @param description Optional description to assign if a new analysis is
#'   created.
#' @param workspace Parent workspace (object or API ID). If `NULL`, the
#'   client's default workspace is used.
#' @param create Logical; if `TRUE` and the analysis does not exist, a new one
#'   is created. If `FALSE`, an error is thrown when no matching analysis is
#'   found.
#'
#' @return An analysis object corresponding to the matching (or newly created)
#'   analysis.
#' @export
find_analysis <- function(gf, name, description=NULL, workspace=NULL, create=FALSE) {
  worx <- get_workspace(gf, infer_workspace(gf, workspace))
  find_or_create(gf, name, create=create,
                 type="analysis",
                 get_list=function() { worx$analyses },
                 do_create=function() {
                   create_analysis(gf, name, description, workspace=get_api_id(worx))
                 })
}
