library(httr)
library(jsonlite)
library(data.table)

API_URL = "http://localhost:8000"
API_VERSION = "v1.2"

APP_URL = "https://app.gofigr.io"

gofigr.client <- function(username=NULL, password=NULL, api_key=NULL,
                  url=API_URL, anonymous=FALSE, verbose=FALSE) {
  client <- structure(
    local({username=username
           password=password
           api_key=api_key
           url=paste0(API_URL, "/api/", API_VERSION, "/")
           jwt_url=paste0(API_URL, "/api/token/")
           anonymous=anonymous
           access_token=NULL
           refresh_token=NULL
           verbose=verbose
           environment()
                 }))
  class(client) <- "gofigr"
  return(client)
}

print.gofigr <- function(gf) {
  cat(paste0("GoFigr client at ", gf$url, "\n"))
}

gofigr.cat <- function(gf, content, ...) {
  if(gf$verbose) {
    cat(paste0(content, "\n"), ...)
  }
}

authenticate_jwt <- function(gf) {
  if(is.null(gf$username) || is.null(gf$password)) {
    stop("GoFigr username and password cannot be null when using JWT authentication")
  }

  res <- httr::POST(gf$jwt_url,
                    body=jsonlite::toJSON(list(username=gf$username,
                                               password=gf$password),
                                          auto_unbox=TRUE),
                    content_type_json())

  if(res$status_code != 200) {
    stop("Authentication failed")
  }

  res_data <- fromJSON(rawToChar(res$content))

  gf$access_token <- res_data$access
  gf$refresh_token <- res_data$refresh

  gofigr.cat(gf, "JWT authentication successful")
}

refresh_jwt <- function(gf) {
  if(is.null(gf$refresh_token)) {
    stop("JWT refresh token is null")
  }

  res <- httr::POST(paste0(gf$jwt_url, "refresh/"),
                    body=jsonlite::toJSON(list(refresh=gf$refresh_token),
                                          auto_unbox=TRUE),
                    content_type_json())

  if(res$status_code == 200) {
    res_data <- fromJSON(rawToChar(res$content))
    gf$access_token <- res_data$access

    gofigr.cat(gf, "JWT refresh successful")
  } else {
    gofigr.cat(gf, "JWT refresh failed. Attempting re-authentication.")
    authenticate_jwt(gf)
  }
}

is_expired_token <- function(res) {
  if(res$status_code != 401) { # UNAUTHORIZED
    return(FALSE)
  }

  tryCatch({
    obj <- fromJSON(rawToChar(res$content))
    return(obj$code == "token_not_valid")
  })

  return(FALSE)
}

gofigr.make_handler <- function(method) {
  function(gf, url, expected_status_code=200, ...) {
    full_url <- paste0(gf$url, url)

    if(gf$anonymous) {
      return(method(full_url))
    } else if(!is.null(gf$api_key)) {
      # API key auth

      return(method(full_url,
                    add_headers(Authorization = paste0('Token ', gf$api_key)),
                    ...))
    } else {
      # JWT

      if(is.null(gf$access_token)) {
        # Initial authentication
        authenticate_jwt(gf)
      }

      # Try the access token
      res <- method(full_url,
                    add_headers(Authorization = paste0('Bearer ', gf$access_token)),
                    ...)

      if(is_expired_token(res)) {  # Token expired?
        gofigr.cat(gf, "Token expired. Trying refresh.")
        refresh_jwt(gf)

        res <- method(full_url,
                      add_headers(Authorization = paste0('Bearer ', gf$access_token)),
                      ...)

        if(res$status_code != expected_status_code) {
          stop(paste0("Request to ", full_url, " returned ", res$status_code,
                      ": ",
                      rawToChar(res$content)))
        }
      }

      return(res)
    }
  }
}

gofigr.GET <- gofigr.make_handler(httr::GET)
gofigr.POST <- gofigr.make_handler(httr::POST)
gofigr.PUT <- gofigr.make_handler(httr::PUT)
gofigr.DELETE <- gofigr.make_handler(httr::DELETE)


gf <- gofigr.client(username=Sys.getenv("GF_USERNAME"),
                    password=Sys.getenv("GF_PASSWORD"),
                    verbose=TRUE)

for(x in 1:10) {
  print(x)
  print(fromJSON(rawToChar(gofigr.GET(gf, "workspace/")$content))$name)
  Sys.sleep(1)
}
