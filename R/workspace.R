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


#' Resolve the workspace to use, falling back to the client's default.
#'
#' Resolves a concrete workspace in this order of precedence:
#' \enumerate{
#'   \item the explicit `workspace` argument, if supplied;
#'   \item a workspace matched (or created) by `workspace_name`, if supplied;
#'   \item the default workspace configured on the GoFigr client;
#'   \item the single workspace accessible to the client, when exactly one is
#'     visible. This mirrors the Python client and is what scoped API keys
#'     (e.g. on compute instances, whose credentials carry no workspace) rely
#'     on.
#' }
#' Throws an error if none of these resolve to a workspace.
#'
#' @param gf GoFigr client.
#' @param workspace Optional workspace object or API ID. Takes precedence over
#'   all other arguments.
#' @param workspace_name Optional workspace name to look up (or create, when
#'   `create_workspace` is `TRUE`).
#' @param create_workspace Logical; if `TRUE` and `workspace_name` does not
#'   match an existing workspace, a new one is created.
#' @param workspace_description Optional description used when creating a
#'   workspace by name.
#'
#' @return A workspace API ID suitable for passing to other helpers.
#' @export
infer_workspace <- function(gf, workspace=NULL, workspace_name=NULL,
                            create_workspace=FALSE, workspace_description=NULL) {
  if(!is.null(workspace)) {
    return(workspace)
  } else if(!is.null(workspace_name)) {
    return(find_workspace(gf, workspace_name,
                          description=workspace_description,
                          create=create_workspace)$api_id)
  } else if(!is.null(gf$workspace)) {
    return(gf$workspace)
  }

  # No workspace specified: fall back to the single accessible workspace. This
  # is the scoped-API-key case (e.g. compute instances), where the server
  # resolves the key to exactly one workspace.
  available <- list_workspaces(gf)
  if(length(available) == 1) {
    return(available[[1]]$api_id)
  } else if(length(available) == 0) {
    stop("Workspace not specified and no workspaces are accessible to this API key.")
  } else {
    stop("Workspace not specified and no default workspace available. ",
         "Please specify either workspace (API ID) or workspace_name.")
  }
}
