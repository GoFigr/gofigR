#' List of data types supported by GoFigr
#'
#' @return list where names are human-readable names of data types, and values
#'  are corresponding API types (strings).
#' @export
DATA_TYPES <- list(
  data_frame = "dataframe",
  code = "code",
  image = "image",
  text = "text",
  file = "file")

#' Fetches a revision given an API ID.
#'
#' @param gf GoFigr client
#' @param api_id API ID for the revision
#'
#' @return revision object
#' @export
get_revision <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("revision/", api_id)))
}

#' Gets the full URL for a revision
#'
#' @param rev revision object
#'
#' @return URL, a string
#' @export
get_revision_url <- function(rev) {
  if(is.null(rev) || is.null(rev$api_id)) {
    return(NULL)
  }

  if("client" %in% names(rev)) {
    base_url <- gsub("api", "app", rev$client$base_url)
  } else {
    base_url <- APP_URL
  }

  id <- default_if_null(rev$short_id, rev$api_id)
  paste0(base_url, "/r/", id)
}

#' Creates a new revision
#'
#' @param gf GoFigr client
#' @param figure figure under which to create the revision
#' @param metadata metadata for the revision, as a named list
#' @param data list of Data objects
#' @param client_id optional client-generated UUID for idempotent creation.
#'   When provided, the server uses this as the revision's ID and returns
#'   409 Conflict if a revision with the same client_id already exists.
#' @param short_id optional short ID for compact QR codes. Generated via
#'   \code{\link{reserve_short_id_prefix}} and \code{make_short_id}.
#'
#' @return created revision object
#' @export
create_revision <- function(gf, figure, metadata=list(), data=NULL,
                            client_id=NULL, short_id=NULL) {
  if(is.null(data)) {
    data <- list()
  }

  body <- list(figure=get_api_id(figure),
               metadata=metadata,
               data=lapply(data, encode_raw_data))

  if(!is.null(client_id)) {
    body$client_id <- client_id
  }
  if(!is.null(short_id)) {
    body$short_id <- short_id
  }

  response_to_JSON(gofigr_POST(gf, "revision/",
                               body=obj_to_JSON(body),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Creates a new revision via the auto-assign endpoint.
#'
#' The server creates a temporary figure, processes the image data, then uses
#' AI to assign the revision to the correct figure. The revision will have
#' \code{is_processing=TRUE} until assignment completes.
#'
#' @param gf GoFigr client
#' @param analysis analysis under which to auto-assign the revision
#' @param metadata metadata for the revision, as a named list
#' @param data list of Data objects
#' @param client_id optional client-generated UUID for idempotent creation
#' @param short_id optional short ID for compact QR codes
#'
#' @return created revision object
#' @export
create_revision_auto_assign <- function(gf, analysis, metadata=list(), data=NULL,
                                        client_id=NULL, short_id=NULL) {
  if(is.null(data)) {
    data <- list()
  }

  body <- list(analysis=get_api_id(analysis),
               metadata=metadata,
               data=lapply(data, encode_raw_data))

  if(!is.null(client_id)) {
    body$client_id <- client_id
  }
  if(!is.null(short_id)) {
    body$short_id <- short_id
  }

  response_to_JSON(gofigr_POST(gf, "revision/auto-assign/",
                               body=obj_to_JSON(body),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Updates data associated with a figure
#'
#' @param gf GoFigr client
#' @param revision revision or its API ID for which to update the data
#' @param new_data new data, as a list of GoFigrData objects (e.g. make_image_data or make_text_data)
#' @param silent whether to generate an activity. Internal use only.
#' @param assets list of asset revision IDs to be assocaited with this revision
#' @param is_clean_room if TRUE, marks the revision as a clean room revision
#'
#' @return updated revision
#' @export
update_revision_data <- function(gf, revision, new_data, silent=FALSE,
                                 assets=list(), is_clean_room=NULL) {
  if(is.character(revision)) {
    revision <- get_revision(gf, revision)
  }

  if(silent) {
    params <- "?silent=true"
  } else {
    params <- ""
  }

  asset_data <- lapply(assets, function(ar) {asset_linked_to_figure(revision, ar)})

  patch_body <- list(data=lapply(new_data, encode_raw_data),
                     assets=asset_data)

  if (isTRUE(is_clean_room)) {
    patch_body$is_clean_room <- TRUE
  }

  response_to_JSON(gofigr_PATCH(gf, paste0("revision/", get_api_id(revision), "/", params),
                               body=obj_to_JSON(patch_body),
                               httr::content_type_json(),
                               expected_status_code = 200))
}
