#' Retrieves workspace details.
#'
#' @param gf GoFigr client
#' @param api_id API ID of the workspace
#'
#' @return workspace details
#' @export
get_workspace <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("workspace/", api_id)))
}

#' List all workspaces available to the user.
#'
#' @param gf GoFigr client
#'
#' @return List of workspaces
#' @export
list_workspaces <- function(gf) {
  response_to_JSON(gofigr_GET(gf, "workspace/"))
}

#' Creates a new workspace
#'
#' @param gf GoFigr client
#' @param name workspace name
#' @param description workspace description
#'
#' @return created workspace object
#' @export
create_workspace <- function(gf, name, description=NULL) {
  response_to_JSON(gofigr_POST(gf, "workspace/",
                               body=obj_to_JSON(list(name=name,
                                                     description=null_to_empty(description))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

#' Finds a workspace by name, optionally creating it if doesn't exist
#'
#' @param gf GoFigr client
#' @param name name of the workspace to find
#' @param description workspace description if creating a new one
#' @param create if TRUE and workspace is not found, it will be created
#'
#' @return workspace if found; throws an error if not.
#' @export
find_workspace <- function(gf, name, description=NULL, create=FALSE) {
  find_or_create(gf, name, create=create,
                 type="workspace",
                 get_list=function() { list_workspaces(gf) },
                 do_create=function() {
                   create_workspace(gf, name, description)
                 })
}


#' Returns the argument if a valid workspace is passed, or the default
#' workspace from the GoFigr client otherwise. Throws an error if both
#' are NULL.
#'
#' @param gf GoFigr client
#' @param workspace workspace or NULL
#'
#' @return workspace object
#' @export
infer_workspace <- function(gf, workspace) {
  if(!is.null(workspace)) {
    return(workspace)
  }
  else if(!is.null(gf$workspace)) {
    return(gf$workspace)
  } else {
    stop("Workspace not specified and no default workspace available.")
  }
}
