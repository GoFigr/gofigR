#' Creates a GoFigr data object which can be attached to revisions.
#'
#' @param name name of this data
#' @param type data type, e.g. DATA_TYPES$image
#' @param metadata metadata associated with this data object
#' @param data raw bytes
#'
#' @return data object
#' @export
make_raw_data <- function(name, type, metadata, data) {
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
make_text_data <- function(name, contents, metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }
  metadata$encoding <- "utf-8"
  return(make_raw_data(name, DATA_TYPES$text,
                       metadata=metadata, data=charToRaw(enc2utf8(contents))))
}

#' Creates a GoFigr data object storing source code
#'
#' @param name name of this code object
#' @param contents_or_file contents, a character string or file object
#' @param language programming language, e.g. Python or R
#' @param format not supported at the moment; please use the default
#' @param metadata metadata associated with this object
#'
#' @return GoFigr data object
#' @export
make_code_data <- function(name, contents_or_file, language, format="text",
                           metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  if(inherits(contents_or_file, 'connection')) {
    contents <- readr::read_file(contents_or_file)
  } else {
    contents <- contents_or_file
  }

  metadata$encoding <- "utf-8"
  metadata$language <- language
  metadata$format <- format

  return(make_raw_data(name, DATA_TYPES$code,
                       metadata=metadata, data=charToRaw(enc2utf8(contents))))
}

#' Creates a GoFigr data object storing image data
#'
#' @param name name of this image
#' @param file_or_raw image data, either a file or a raw vector
#' @param format format, e.g. "png"
#' @param is_watermarked whether this file has a GoFigr watermark
#' @param metadata metadata associated with this image
#'
#' @return GoFigr data object
#' @export
make_image_data <- function(name, file_or_raw, format, is_watermarked,
                            metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  metadata$format <- format
  metadata$is_watermarked <- is_watermarked

  if(inherits(file_or_raw, "connection") || is.character(file_or_raw)) {
    data <- readr::read_file_raw(file_or_raw)
  } else if(is.raw(file_or_raw)) {
    data <- file_or_raw
  } else {
    stop("Unsupported image data input. Please supply a file, a file path, or a raw vector.")
  }

  return(make_raw_data(name, DATA_TYPES$image,
                       metadata=metadata, data=data))
}

image_to_html <- function(data) {
  contents <- base64enc::base64encode(data$data)
  return(paste0("<img src=\"",
                "data:image;base64,",
                contents,
                "\"/>"))
}

#' Creates a GoFigr data object storing file data
#'
#' @param name name of this file
#' @param file_or_raw image data, either a file or a raw vector
#' @param path file path
#' @param metadata metadata associated with this file
#'
#' @return GoFigr data object
#' @export
make_file_data <- function(name, file_or_raw, path=NULL, metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  if(is.character(file_or_raw)) {
    path <- file_or_raw
  }

  metadata$path <- path

  if(inherits(file_or_raw, "connection") || is.character(file_or_raw)) {
    data <- readr::read_file_raw(file_or_raw)
  } else if(is.raw(file_or_raw)) {
    data <- file_or_raw
  } else {
    stop("Unsupported image data input. Please supply a file, a file path, or a raw vector.")
  }

  return(make_raw_data(name, DATA_TYPES$file,
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
make_table_data <- function(name, frame, metadata=NULL) {
  if(is.null(metadata)) {
    metadata <- list()
  }

  metadata$format <- "csv"
  metadata$encoding <- "utf-8"

  return(make_raw_data(name, DATA_TYPES$data_frame, metadata=metadata,
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
#' data <- make_raw_data("test", "text", list(a=1), charToRaw("abcdefksjdfklsd"))
#' encode_raw_data(data)
encode_raw_data <- function(data) {
  if(identical(data$data, raw(0))) {
    datum <- ""
  } else {
    datum <- base64enc::base64encode(data$data)
  }

  return(list(
    name=data$name,
    type=data$type,
    metadata=data$metadata,
    data=datum
  ))
}

#' Default print representation of GoFigr data objects.
#'
#' @param x object to print
#' @param ... passed to cat
#'
#' @return NA
#' @export
print.gofigrdata <- function(x, ...) {
  cat(paste0("GoFigr data (\"", x$name, "\", ", x$type, "): ", length(x$data), " bytes"), ...)
}
