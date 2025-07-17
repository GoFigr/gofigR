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

  paste0(base_url, "/r/", rev$api_id)
}

#' Creates a new revision
#'
#' @param gf GoFigr client
#' @param figure figure under which to create the revision
#' @param metadata metadata for the revision, as a named list
#' @param data list of Data objects
#'
#' @return created revision object
#' @export
create_revision <- function(gf, figure, metadata=list(), data=NULL) {
  if(is.null(data)) {
    data <- list()
  }

  response_to_JSON(gofigr_POST(gf, "revision/",
                               body=obj_to_JSON(list(figure=get_api_id(figure),
                                                     metadata=metadata,
                                                     data=lapply(data, encode_raw_data))),
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
#'
#' @return updated revision
#' @export
update_revision_data <- function(gf, revision, new_data, silent=FALSE,
                                 assets=list()) {
  if(is.character(revision)) {
    revision <- get_revision(gf, revision)
  }

  if(silent) {
    params <- "?silent=true"
  } else {
    params <- ""
  }

  asset_data <- lapply(assets, function(ar) {asset_linked_to_figure(revision, ar)})

  response_to_JSON(gofigr_PATCH(gf, paste0("revision/", get_api_id(revision), "/", params),
                               body=obj_to_JSON(list(data=lapply(new_data, encode_raw_data),
                                                     assets=asset_data)),
                               httr::content_type_json(),
                               expected_status_code = 200))
}
