#' Retrieve workspace details.
#'
#' @param gf GoFigr client created by `gofigr_client()`.
#' @param api_id Character string with the API ID of the workspace to fetch.
#'
#' @return A workspace object as returned by the API, including metadata and
#'   lists of analyses and members.
#' @export
get_workspace <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("workspace/", api_id, "/")))
}

#' List all workspaces available to the authenticated user.
#'
#' @param gf GoFigr client.
#'
#' @return A list of workspace objects visible to the current user.
#' @export
list_workspaces <- function(gf) {
  response_to_JSON(gofigr_GET(gf, "workspace/"))
}

#' Create a new workspace.
#'
#' Workspaces are top-level containers for analyses and figures. This helper
#' creates a new workspace owned by the current user.
#'
#' @param gf GoFigr client.
#' @param name Human-readable workspace name.
#' @param description Optional longer description of the workspace.
#'
#' @return The created workspace object as returned by the API.
#' @export
create_workspace <- function(gf, name, description=NULL) {
  response_to_JSON(gofigr_POST(gf, "workspace/",
                               body=obj_to_JSON(list(name=name,
                                                     description=null_to_empty(description))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

#' Find a workspace by name, optionally creating it.
#'
#' Searches the workspaces visible to the current user by name and, optionally,
#' creates a new workspace when no match is found.
#'
#' @param gf GoFigr client.
#' @param name Name of the workspace to find.
#' @param description Optional description to use if a new workspace is
#'   created.
#' @param create Logical; if `TRUE` and the workspace does not exist, a new
#'   one is created. If `FALSE`, an error is thrown when no matching workspace
#'   is found.
#'
#' @return A workspace object corresponding to the matching (or newly created)
#'   workspace.
#' @export
find_workspace <- function(gf, name, description=NULL, create=FALSE) {
  find_or_create(gf, name, create=create,
                 type="workspace",
                 get_list=function() { list_workspaces(gf) },
                 do_create=function() {
                   create_workspace(gf, name, description)
                 })
}


#' Resolve the workspace argument, falling back to the client's default.
#'
#' Returns the supplied workspace if present, otherwise the default workspace
#' configured on the GoFigr client. Throws an error if neither is available.
#'
#' @param gf GoFigr client.
#' @param workspace Optional workspace object or API ID. If `NULL`, the
#'   client's default workspace is used.
#'
#' @return A workspace object or API ID suitable for passing to other helpers.
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
