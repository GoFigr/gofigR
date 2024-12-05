#' Creates a GoFigr data object which can be attached to revisions.
#'
#' @param name name of this data
#' @param type data type, e.g. DATA.TYPES$image
#' @param metadata metadata associated with this data object
#' @param data raw bytes
#'
#' @return data object
#' @export
make.raw.data <- function(name, type, metadata, data) {
  obj <- list(name=name,
              type=type,
              metadata=metadata,
              data=data)
  class(obj) <- "gofigrdata"
  return(obj)
}

#' Creates a GoFigr data object to store text
#'
#' @param name name of this data object
#' @param contents contents, a character string
#' @param metadata metadata associated with this object
#'
#' @return GoFigr data object
#' @export
make.text.data <- function(name, contents, metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }
  metadata$encoding <- "utf-8"
  return(make.raw.data(name, DATA.TYPES$text,
                       metadata=metadata, data=charToRaw(enc2utf8(contents))))
}

#' Creates a GoFigr data object storing source code
#'
#' @param name name of this code object
#' @param contents contents, a character string or file object
#' @param language programming language, e.g. Python or R
#' @param format not supported at the moment; please use the default
#' @param metadata metadata associated with this object
#'
#' @return GoFigr data object
#' @export
make.code.data <- function(name, contents.or.file, language, format="text",
                           metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  if(inherits(contents.or.file, 'connection')) {
    contents <- readr::read_file(contents.or.file)
  } else {
    contents <- contents.or.file
  }

  metadata$encoding <- "utf-8"
  metadata$language <- language
  metadata$format <- format

  return(make.raw.data(name, DATA.TYPES$code,
                       metadata=metadata, data=charToRaw(enc2utf8(contents))))
}

#' Creates a GoFigr data object storing image data
#'
#' @param name name of this image
#' @param file.or.raw image data, either a file or a raw vector
#' @param format format, e.g. "png"
#' @param is.watermarked whether this file has a GoFigr watermark
#' @param metadata metadata associated with this image
#'
#' @return GoFigr data object
#' @export
make.image.data <- function(name, file.or.raw, format, is.watermarked,
                            metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  metadata$format <- format
  metadata$is_watermarked <- is.watermarked # not a typo -- API expects an underscore

  if(inherits(file.or.raw, "connection") || is.character(file.or.raw)) {
    data <- readr::read_file_raw(file.or.raw)
  } else if(is.raw(file.or.raw)) {
    data <- file.or.raw
  } else {
    stop("Unsupported image data input. Please supply a file, a file path, or a raw vector.")
  }

  return(make.raw.data(name, DATA.TYPES$image,
                       metadata=metadata, data=data))
}

#' Creates a GoFigr data object storing data.frame/tabular data
#'
#' @param name name of this data object
#' @param frame data.frame
#' @param metadata metadata associated with this data object
#'
#' @return GoFigr data object
#' @export
make.table.data <- function(name, frame, metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  metadata$format <- "csv"
  metadata$encoding <- "utf-8"

  return(make.raw.data(name, DATA.TYPES$data.frame, metadata=metadata,
                       data=charToRaw(enc2utf8(readr::format_csv(frame)))))
}

#' Converts a GoFigr data object into R primitives that can be converted to JSON,
#' performing base64 encoding of binary data.
#'
#' @param data GoFigr data object
#'
#' @return encoded data object
#' @export
#'
#' @examples
#' data <- make.raw.data("test", "text", list(a=1), charToRaw("abcdefksjdfklsd"))
#' encode.raw.data(data)
encode.raw.data <- function(data) {
  return(list(
    name=data$name,
    type=data$type,
    metadata=data$metadata,
    data=base64encode(data$data)
  ))
}

#' Default print representation of GoFigr data objects.
#'
#' @param obj object to print
#' @param ... passed to cat
#'
#' @return NA
#' @export
print.gofigrdata <- function(obj, ...) {
  cat(paste0("GoFigr data (\"", obj$name, "\", ", obj$type, "): ", length(obj$data), " bytes"), ...)
}
