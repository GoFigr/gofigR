#' Fetches an asset given an API ID.
#'
#' @param gf GoFigr client
#' @param api_id API ID for the asset
#'
#' @return asset object
#' @export
get_asset <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("asset/", api_id)))
}



#' Creates a new asset
#'
#' @param gf GoFigr client
#' @param workspace parent workspace
#' @param name name of the asset
#' @param description description of the asset
#'
#' @returns asset object
#' @export
create_asset <- function(gf, workspace, name, description=NULL) {
  response_to_JSON(gofigr_POST(gf, "asset/",
                               body=obj_to_JSON(list(workspace=get_api_id(workspace),
                                                     name=name,
                                                     description=null_to_empty(description))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Finds an asset by name
#'
#' @param gf GoFigr client
#' @param name name of the asset to search for
#'
#' @returns list of matching assets, or an empty list if none found
#' @export
find_asset_by_name <- function(gf, name) {
  response_to_JSON(gofigr_POST(gf, "asset/find_by_name/",
                   body=obj_to_JSON(list(name=name)),
                   httr::content_type_json(),
                   expected_status_code = 200))
}


#' Gets an asset revision given an API ID
#'
#' @param gf GoFigr client
#' @param api_id API ID for the revision
#'
#' @returns asset revision object
#' @export
get_asset_revision <- function(gf, api_id) {
  response_to_JSON(gofigr_GET(gf, paste0("asset_revision/", api_id)))
}



#' Creates a new asset revision
#'
#' @param gf GoFigr client
#' @param asset asset under which to create the revision
#' @param metadata metadata for the revision, as a named list
#' @param data list of Data objects
#'
#' @return created revision object
#' @export
create_asset_revision <- function(gf, asset, metadata=list(), data=NULL) {
  if(is.null(data)) {
    data <- list()
  }

  response_to_JSON(gofigr_POST(gf, "asset_revision/",
                               body=obj_to_JSON(list(asset=get_api_id(asset),
                                                     metadata=metadata,
                                                     data=lapply(data, encode_raw_data))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Finds all asset revisions with a matching hash digest
#'
#' @param gf GoFigr client
#' @param digest hex digest string
#' @param hash_type digest type
#'
#' @returns list of asset revisions, or empty list
#' @export
find_asset_revision_by_hash <- function(gf, digest, hash_type="blake3") {
  if(is.null(digest)) {
    stop("Digest cannot be null")
  } else if(is.null(hash_type)) {
    stop("Hash type cannot be null")
  }

  response_to_JSON(gofigr_POST(gf, "asset_revision/find_by_hash/",
                               body=obj_to_JSON(list(digest=digest,
                                                     hash_type=hash_type)),
                               httr::content_type_json(),
                               expected_status_code = 200))
}


#' Calculates a checksum for a file
#'
#' @param path path to the file
#'
#' @returns checksum, as a hex digest
calc_checksum <- function(path) {
  digest::digest(path, algo="blake3", file=TRUE)
}


#' Creates a new asset revision from file.
#'
#' @param gf GoFigr client
#' @param workspace_id parent workspace in case we have to create a brand new asset
#' @param path path to file
#'
#' @returns asset revision object
new_asset_revision_from_file <- function(gf, workspace_id, path) {
  # Find existing assets to create this revision under
  name <- basename(path)
  assets <- find_asset_by_name(gf, name)

  if(length(assets) == 0) {
    parent_asset <- create_asset(gf, workspace_id, name)
  } else if(length(assets) == 1) {
    parent_asset <- assets[[1]]
  } else {
    warning("Multiple assets with the same name found. Default to first: ",
            paste0(sapply(assets, function(x) {x$api_id}), collapse=", "))
    parent_asset <- assets[[1]]
  }

  rev <- create_asset_revision(gf, parent_asset, list(path=path),
                               data=list(make_file_data(name, path)))
  return(rev)
}


#' Syncs a file with the GoFigr service
#'
#' \enumerate{
#'   \item If we haven't seen this file before, creates a new asset and a new revision
#'   \item If we have seen the file but haven't seen this revision, creates a new revision
#'   \item If we have seen this revision, returns the existing revision
#'   }
#'
#' @param gf GoFigr client
#' @param workspace_id parent workspace in case we have to create a brand new asset
#' @param path path to file
#'
#' @returns asset revision object
#' @export
sync_workspace_asset <- function(gf, workspace_id, path) {
  checksum <- calc_checksum(path)

  # Find existing asset revisions with this checksum
  revisions = find_asset_revision_by_hash(gf, checksum)

  if(length(revisions) == 0) {
    return(new_asset_revision_from_file(gf, workspace_id, path))
  } else if(length(revisions) == 1) {
    return(revisions[[1]])
  } else {
    warning("Multiple revisions with the same checksum found. Default to first: ",
            paste0(sapply(revisions, function(x) {x$api_id}), collapse=", "))
    return(revisions[[1]])
  }
}


#' Creates an object representing a relationship between a figure and
#' an asset.
#'
#' @param figure_revision figure revision ID or object
#' @param asset_revision asset revision ID or object
#' @param use_type direct or indirect
#'
#' @returns relationship object
#' @export
asset_linked_to_figure <- function(figure_revision, asset_revision,
                                   use_type="indirect") {
  return(list(figure_revision=get_api_id(figure_revision),
              asset_revision=get_api_id(asset_revision),
              use_type=use_type))
}

