#' Lists analyses under a workspace.
#'
#' @param gf GoFigr client
#' @param workspace_id API id of the workspace
#'
#' @return list of analyses
#' @export
list_analyses <- function(gf, workspace_id=NULL) {
  worx <- get_workspace(gf, default_if_null(workspace_id, gf$workspace))
  return(worx$analyses)
}

#' Creates a new analysis
#'
#' @param gf GoFigr client
#' @param name analysis name
#' @param description analysis description
#' @param workspace analysis will be created under this workspace. Can be a workspace object or an API ID.
#'
#' @return created analysis
#' @export
create_analysis <- function(gf, name, description=NULL, workspace=NULL) {
  response_to_JSON(gofigr_POST(gf, "analysis/",
                               body=obj_to_JSON(list(name=name,
                                                     description=null_to_empty(description),
                                                     workspace=infer_workspace(gf, workspace))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Fetches an analysis given an API ID.
#'
#' @param gf GoFigr client
#' @param api_id API ID for the analysis
#'
#' @return analysis object
#' @export
get_analysis <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("analysis/", api_id)))
}


#' Finds an analysis by name, optionally creating one if it doesn't exist.
#'
#' @param gf GoFigr client
#' @param name name of the analysis to find
#' @param description description of the analysis if it needs to be created
#' @param workspace parent workspace
#' @param create if TRUE and the analysis doesn't exist, it will be created; throws an error otherwise.
#'
#' @return analysis object
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
