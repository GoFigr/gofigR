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
#' @param analysis optional analysis API ID to scope the asset under
#'
#' @returns asset object
#' @export
create_asset <- function(gf, workspace, name, description=NULL, analysis=NULL) {
  payload <- list(workspace=get_api_id(workspace),
                  name=name,
                  description=null_to_empty(description))
  if(!is.null(analysis)) {
    payload$analysis <- analysis
  }
  response_to_JSON(gofigr_POST(gf, "asset/",
                               body=obj_to_JSON(payload),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Finds an asset by name
#'
#' @param gf GoFigr client
#' @param name name of the asset to search for
#' @param analysis optional analysis API ID to scope the search
#'
#' @returns list of matching assets, or an empty list if none found
#' @export
find_asset_by_name <- function(gf, name, analysis=NULL) {
  payload <- list(name=name)
  if(!is.null(analysis)) {
    payload$analysis <- analysis
  }
  response_to_JSON(gofigr_POST(gf, "asset/find_by_name/",
                   body=obj_to_JSON(payload),
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
#' @param analysis optional analysis API ID to scope the search
#'
#' @returns list of asset revisions, or empty list
#' @export
find_asset_revision_by_hash <- function(gf, digest, hash_type="blake3", analysis=NULL) {
  if(is.null(digest)) {
    stop("Digest cannot be null")
  } else if(is.null(hash_type)) {
    stop("Hash type cannot be null")
  }

  payload <- list(digest=digest, hash_type=hash_type)
  if(!is.null(analysis)) {
    payload$analysis <- analysis
  }

  response_to_JSON(gofigr_POST(gf, "asset_revision/find_by_hash/",
                               body=obj_to_JSON(payload),
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
#' @param analysis_id optional analysis API ID to scope the asset under
#'
#' @returns asset revision object
new_asset_revision_from_file <- function(gf, workspace_id, path, analysis_id=NULL) {
  # Find existing assets to create this revision under
  name <- basename(path)
  assets <- find_asset_by_name(gf, name, analysis=analysis_id)

  if(length(assets) == 0) {
    parent_asset <- create_asset(gf, workspace_id, name, analysis=analysis_id)
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
#' @param analysis_id optional analysis API ID to scope the asset under
#'
#' @returns asset revision object
#' @export
sync_workspace_asset <- function(gf, workspace_id, path, analysis_id=NULL) {
  checksum <- calc_checksum(path)

  # Find existing asset revisions with this checksum
  revisions = find_asset_revision_by_hash(gf, checksum, analysis=analysis_id)

  if(length(revisions) == 0) {
    return(new_asset_revision_from_file(gf, workspace_id, path, analysis_id=analysis_id))
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



#' Materializes a GoFigr asset revision as a local file and registers it as
#' an input of every figure published afterwards: the asset-first counterpart
#' of sync_file(). Nothing is re-hashed or re-uploaded; the revision is
#' already known, so the provenance link is exact.
#'
#' The file is written under `directory` (default ./data) with the name it was
#' uploaded with, or at `path` if given. A file already there with the
#' revision's blake3 digest is reused without downloading. The digest of what
#' was written is verified against the server's.
#'
#' @param revision asset revision object or API ID
#' @param path where to write the file (default: <directory>/<original file name>)
#' @param directory directory for the default path (created if missing)
#' @param quiet suppress the "Asset in use" message
#'
#' @returns path to the local file
#' @export
use_asset <- function(revision, path=NULL, directory="data", quiet=FALSE) {
  if(!check_configured(response=stop)) {
    stop("GoFigr is not configured. Call gofigR::enable() first.")
  }
  opts <- get_options()
  gf <- opts$client
  rev <- if(is.character(revision)) get_asset_revision(gf, revision) else revision
  if(is.null(rev$data) || length(rev$data) == 0) {
    rev <- get_asset_revision(gf, get_api_id(rev))
  }
  if(length(rev$data) == 0) {
    stop(paste0("Asset revision ", rev$api_id, " has no file data."))
  }
  datum <- rev$data[[1]]
  file_name <- if(!is.null(datum$name) && nzchar(datum$name)) datum$name else paste0("asset-", rev$api_id)
  target <- if(is.null(path)) file.path(directory, file_name) else path
  target_dir <- dirname(target)
  if(nzchar(target_dir) && !dir.exists(target_dir)) {
    dir.create(target_dir, recursive=TRUE)
  }

  expected <- datum$hash
  if(file.exists(target) && !is.null(expected) && identical(calc_checksum(target), expected)) {
    message(paste0("Using cached ", target, " for asset revision ", rev$api_id))
  } else {
    full <- get_data(gf, datum$api_id)
    bytes <- if(is.null(full$data) || !nzchar(full$data)) raw(0) else base64enc::base64decode(full$data)
    writeBin(bytes, target)
    actual <- calc_checksum(target)
    if(!is.null(expected) && !identical(actual, expected)) {
      unlink(target)
      stop(paste0("Downloaded data for asset revision ", rev$api_id, " did not match its digest (",
                  actual, " != ", expected, ")."))
    }
  }

  opts$assets[[get_api_id(rev)]] <- rev
  if(!quiet) {
    asset_url <- paste0(get_app_url(gf), "/asset_revision/", rev$api_id)
    gf_print_url(asset_url, paste0("Asset in use: ", asset_url))
  }
  return(target)
}
