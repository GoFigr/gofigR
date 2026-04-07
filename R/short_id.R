#' @noRd
BASE62_CHARS <- c(0:9, letters, LETTERS)

#' Encode a non-negative integer as a base62 string.
#'
#' @param num non-negative integer to encode
#'
#' @return base62-encoded string
#' @noRd
base62_encode <- function(num) {
  if (num < 0) {
    stop("Cannot encode negative numbers")
  }
  if (num == 0) {
    return("0")
  }

  chars <- character(0)
  while (num > 0) {
    remainder <- num %% 62
    num <- num %/% 62
    chars <- c(chars, as.character(BASE62_CHARS[remainder + 1]))
  }
  paste0(rev(chars), collapse = "")
}


#' Combine a prefix with a base62-encoded index to form a short ID.
#'
#' @param prefix 8-character alphanumeric prefix from the server
#' @param index non-negative integer index
#'
#' @return short ID string
#' @noRd
make_short_id <- function(prefix, index) {
  paste0(prefix, base62_encode(index))
}


#' Reserve a short ID prefix from the GoFigr server.
#'
#' The prefix can be combined with a sequential base62 index to generate
#' compact, unique short IDs for figure revisions. Returns NULL if the
#' server does not support short IDs.
#'
#' @param gf GoFigr client
#'
#' @return 8-character alphanumeric prefix string, or NULL on failure
#' @export
reserve_short_id_prefix <- function(gf) {
  tryCatch({
    res <- gofigr_POST(gf, "short_id_prefix/reserve/",
                       body = "{}",
                       httr::content_type_json(),
                       expected_status_code = 201)
    httr::content(res)$prefix
  }, error = function(e) {
    NULL
  })
}


#' Generate the next short ID using the reserved prefix from GoFigr options.
#'
#' Reads the prefix and counter from the options environment, increments
#' the counter in-place, and returns a short ID string. Returns NULL if
#' no prefix was reserved.
#'
#' @param opts GoFigr options environment (from get_options())
#'
#' @return short ID string, or NULL
#' @noRd
next_short_id <- function(opts) {
  if (is.null(opts$short_id_prefix)) {
    return(NULL)
  }
  sid <- make_short_id(opts$short_id_prefix, opts$short_id_counter)
  opts$short_id_counter <- opts$short_id_counter + 1L
  sid
}
