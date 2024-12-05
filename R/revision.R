DATA.TYPES <- list(
  data.frame = "dataframe",
  code = "code",
  image = "image",
  text = "text")

#' Fetches a revision given an API ID.
#'
#' @param gf GoFigr client
#' @param api_id API ID for the revision
#'
#' @return revision object
#' @export
get.revision <- function(gf, api_id) {
  response.to.JSON(gofigr.GET(gf, paste0("revision/", api_id)))
}

#' Creates a new revision
#'
#' @param gf GoFigr client
#' @param figure figure under which to create the revision
#' @param metadata metadata for the revision, as named list
#' @param data list of Data objects
#'
#' @return created revision object
#' @export
create.revision <- function(gf, figure, metadata=list(), data=NULL) {
  if(is.null(data)) {
    data <- list()
  }

  response.to.JSON(gofigr.POST(gf, "revision/",
                               body=obj.to.JSON(list(figure=get.api.id(figure),
                                                     metadata=metadata,
                                                     data=lapply(data, encode.raw.data))),
                               httr::content_type_json(),
                               expected_status_code = 201))
}


#' Updates data associated with a figure
#'
#' @param gf GoFigr client
#' @param revision revision or its API ID for which to update the data
#' @param new.data new data, as a list of GoFigrData objects (e.g. make.image.data or make.text.data)
#' @param silent whether to generate an activity. Internal use only.
#'
#' @return updated revision
#' @export
update.revision.data <- function(gf, revision, new.data, silent=FALSE) {
  if(is.character(revision)) {
    revision <- get.revision(gf, revision)
  }

  if(silent) {
    params <- "?silent=true"
  } else {
    params <- ""
  }

  response.to.JSON(gofigr.PATCH(gf, paste0("revision/", get.api.id(revision), "/", params),
                               body=obj.to.JSON(list(data=lapply(new.data, encode.raw.data))),
                               httr::content_type_json(),
                               expected_status_code = 200))
}
