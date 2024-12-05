#' Lists analyses under a workspace.
#'
#' @param gf GoFigr client
#' @param workspace_id API id of the workspace
#'
#' @return list of analyses
#' @export
#'
#' @examples
#' list.analyses() # will use default workspace as specified in the GoFigr config
#' list.analyses("59da9bdb-2095-47a9-b414-c029f8a00e0e")
list.analyses <- function(gf, workspace_id=NULL) {
  worx <- get.workspace(gf, default.if.null(workspace_id, gf$workspace))
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
create.analysis <- function(gf, name, description=NULL, workspace=NULL) {
  response.to.JSON(gofigr.POST(gf, "analysis/",
                               body=obj.to.JSON(list(name=name,
                                                     description=null.to.empty(description),
                                                     workspace=infer.workspace(gf, workspace))),
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
get.analysis <- function(gf, api_id) {
  response.to.JSON(gofigr.GET(gf, paste0("analysis/", api_id)))
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
find.analysis <- function(gf, name, description=NULL, workspace=NULL, create=FALSE) {
  worx <- get.workspace(gf, infer.workspace(gf, workspace))
  find.or.create(gf, name, create=create,
                 type="analysis",
                 get.list=function() { worx$analyses },
                 do.create=function() {
                   create.analysis(gf, name, description, workspace=get.api.id(worx))
                 })
}
