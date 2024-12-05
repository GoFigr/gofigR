API_URL = "https://api.gofigr.io"
API_VERSION = "v1.2"

APP_URL = "https://app.gofigr.io"

CONFIG_PATH = file.path(path.expand('~'), ".gofigr")

#' Reads the GoFigr configuration, prioritizing environment variables over the
#' config file:
#'
#' * GF_USERNAME or config["username"]
#' * GF_PASSWORD or config["password"]
#' * GF_API_KEY or config["api_key"]
#' * GF_WORKSPACE or config["workspace"]
#' * GF_URL or config["url"]
#'
#' @param path path to the config file, default ~/.gofigr
#'
#' @return parsed configuration or empty list if not available
#' @export
#'
#' @examples
#' read.config()
#' read.config("~/.gofigr")
read.config <- function(path=CONFIG_PATH) {
  if(!file.exists(path)) {
    return(list())
  }

  tryCatch({
    data <- jsonlite::fromJSON(file(path))

    # Prioritize environment variables
    data$username <- Sys.getenv("GF_USERNAME", unset=default.if.null(data$username, ""))
    data$password <- Sys.getenv("GF_PASSWORD", unset=default.if.null(data$password, ""))
    data$api_key <- Sys.getenv("GF_API_KEY", unset=default.if.null(data$api_key, ""))
    data$workspace <- Sys.getenv("GF_WORKSPACE", unset=default.if.null(data$workspace, ""))
    data$url <- Sys.getenv("GF_URL", unset=default.if.null(data$url, API_URL))

  return(data)
  }, error=function(err) {
    cat(paste0("WARNING: Configuration ", path, " cannot be read: ", err, "\n", file=stderr()))
    return(list())
  })
}

#' Returns a default value if argument is null or empty
#'
#' @param x argument
#' @param default default value if x is null, NA or ""
#'
#' @return x if not null, NA or "", or the default value
default.if.null <- function(x, default) {
  if(is.null(x) || is.na(x) || x == "") {
    return(default)
  } else {
    return(x)
  }
}

#' Creates and configures a GoFigr client. You can login either using
#' a username & password or an API key. See examples.
#'
#' Username, password, API key and workspace are read from the GoFigr
#' configuration file (~/.gofigr) or environment variables if not supplied:
#'
#' * GF_USERNAME or config$username
#' * GF_PASSWORD or config$password
#' * GF_API_KEY or config$api_key
#' * GF_WORKSPACE of config$workspace
#' * GF_URL or config$url
#'
#' @param username username (if not using API key)
#' @param password password (if not using API key)
#' @param api_key API key (if not using password authentication)
#' @param url API URL (optional, you generally won't want to modify this)
#' @param anonymous whether to login anonymously
#' @param verbose set to TRUE to enable verbose output
#' @param workspace default workspace (API ID)
#' @param ignore.config if TRUE, will ignore environment variables and other
#' external configuration
#'
#' @return configured GoFigr client which you can pass to other functions
#' @export
#'
#' @examples
#' gofigr.client()  # use config from ~/.gofigr or environment variables
#' gofigr.client(username="joe", password="abc123") # password login
#' gofigr.client(api_key="abcdef0123456789") # API key login
gofigr.client <- function(username=NULL, password=NULL, api_key=NULL,
                  url=NULL, anonymous=FALSE, verbose=FALSE,
                  workspace=NULL, ignore.config=FALSE) {
  config <- if(ignore.config) list() else read.config()

  api_url <- default.if.null(url, config$url)
  if(is.null(api_url)) {
    api_url <- API_URL
  }

  client <- structure(
    local({username=default.if.null(username, config$username)
           password=default.if.null(password, config$password)
           api_key=default.if.null(api_key, config$api_key)
           url=paste0(api_url, "/api/", API_VERSION, "/")
           jwt_url=paste0(api_url, "/api/token/")
           anonymous=anonymous
           access_token=NULL
           refresh_token=NULL
           verbose=verbose
           workspace=default.if.null(workspace, config$workspace)
           environment()
                 }))

  class(client) <- "gofigr"
  return(client)
}

#' Default print method for a GoFigr client.
#'
#' @param gf GoFigr client
#'
#' @return NA
#' @export
print.gofigr <- function(gf, ...) {
  cat(paste0("GoFigr client at ", gf$url, "\n"))
}

#' Equivalent to cat but only outputs if GoFigr client is verbose.
#'
#' @param gf GoFigr client
#' @param content text to print
#' @param ... passed to cat
#'
#' @return NA
gofigr.cat <- function(gf, content, ...) {
  if(gf$verbose) {
    cat(paste0(content, "\n"), ...)
  }
}

#' Performs JWT authentication with username and password. Saves tokens
#' in the GoFigr client.
#'
#' @param gf GoFigr client
#'
#' @return NA
authenticate_jwt <- function(gf) {
  if(is.null(gf$username) || is.null(gf$password)) {
    stop("GoFigr username and password cannot be null when using JWT authentication")
  }

  res <- httr::POST(gf$jwt_url,
                    body=jsonlite::toJSON(list(username=gf$username,
                                               password=gf$password),
                                          auto_unbox=TRUE),
                    httr::content_type_json())

  if(res$status_code != 200) {
    stop(paste0("Authentication failed: ", rawToChar(res$content)))
  }

  res_data <- jsonlite::fromJSON(rawToChar(res$content))

  gf$access_token <- res_data$access
  gf$refresh_token <- res_data$refresh

  gofigr.cat(gf, "JWT authentication successful")
}

#' Refreshes the JWT access token. Attempts re-authentication if refresh fails.
#'
#' @param gf GoFigr client.
#'
#' @return NA
refresh_jwt <- function(gf) {
  if(is.null(gf$refresh_token)) {
    stop("JWT refresh token is null")
  }

  res <- httr::POST(paste0(gf$jwt_url, "refresh/"),
                    body=jsonlite::toJSON(list(refresh=gf$refresh_token),
                                          auto_unbox=TRUE),
                    httr::content_type_json())

  if(res$status_code == 200) {
    res_data <- jsonlite::fromJSON(rawToChar(res$content))
    gf$access_token <- res_data$access

    gofigr.cat(gf, "JWT refresh successful")
  } else {
    gofigr.cat(gf, "JWT refresh failed. Attempting re-authentication.")
    authenticate_jwt(gf)
  }
}

#' Returns True if the response indicates an expired JWT token
#'
#' @param res httr response
#'
#' @return True if token expired
is.expired.token <- function(res) {
  if(res$status_code != 401) { # UNAUTHORIZED
    return(FALSE)
  }

  tryCatch({
    obj <- jsonlite::fromJSON(rawToChar(res$content))
    return(obj$code == "token_not_valid")
  })

  return(FALSE)
}

#' Convenience function for parsing JSON from httr responses
#'
#' @param response httr response
#'
#' @return parsed JSON
response.to.JSON <- function(response) {
  return(jsonlite::fromJSON(rawToChar(response$content),
                            simplifyDataFrame = FALSE,
                            simplifyMatrix = FALSE,
                            simplifyVector = FALSE))
}

#' Wraps an HTTR method e.g. GET to provide relative URL resolution and
#' authentication
#'
#' @param name method name, e.g. "GET"
#' @param method HTTR method, e.g. httr::GET
#'
#' @return wrapped method which takes a GoFigr client, a relative URL and
#' an expected HTTP status code.
#'
#' @export
#'
#' @examples
#' gofigr.GET <- gofigr.make_handler("GET", httr::GET)
#' responseJSON(gofigr.POST(gf, "api_key/",
#'                          body=jsonlite::toJSON(list(name=name),
#'                                                auto_unbox=TRUE),
#'                          content_type_json(),
#'              expected_status_code = 201))
gofigr.make_handler <- function(name, method) {
  function(gf, url, expected_status_code=200, ...) {
    full_url <- paste0(gf$url, url)
    gofigr.cat(gf, paste0(name, ": ", full_url))

    if(gf$anonymous) {
      res <- method(full_url)
    } else if(!is.null(gf$username) && !is.null(gf$password) && gf$username != "" && gf$password != "") {
      # JWT

      if(is.null(gf$access_token)) {
        # Initial authentication
        authenticate_jwt(gf)
      }

      # Try the access token
      res <- method(full_url,
                    httr::add_headers(Authorization = paste0('Bearer ', gf$access_token)),
                    ...)

      if(is.expired.token(res)) {  # Token expired?
        gofigr.cat(gf, "Token expired. Trying refresh.")
        refresh_jwt(gf)

        res <- method(full_url,
                      httr::add_headers(Authorization = paste0('Bearer ', gf$access_token)),
                      ...)
      }
    } else if(!is.null(gf$api_key)) {
      # API key auth
      res <- method(full_url,
                    httr::add_headers(Authorization = paste0('Token ', gf$api_key)),
                    ...)
    } else {
      stop("No username, password or API key supplied.")
    }

    if(res$status_code != expected_status_code) {
      stop(paste0("Request to ", full_url, " returned ", res$status_code,
                  ": ",
                  rawToChar(res$content)))
    }

    return(res)
  }
}

#' @export
gofigr.GET <- gofigr.make_handler("GET", httr::GET)

#' @export
gofigr.POST <- gofigr.make_handler("POST", httr::POST)

#' @export
gofigr.PUT <- gofigr.make_handler("PUT", httr::PUT)

#' @export
gofigr.PATCH <- gofigr.make_handler("PATCH", httr::PATCH)

#' @export
gofigr.DELETE <- gofigr.make_handler("DELETE", httr::DELETE)


#' Fetches user details for the currently logged in user.
#'
#' @param gf GoFigr client
#'
#' @return user details
#' @export
#'
#' @examples
#' user.info()
user.info <- function(gf) {
  response.to.JSON(gofigr.GET(gf, "user/"))[[1]]
}

#' Creates a new API key. This function will only succeed if using password
#' authentication.
#'
#' @param gf GoFigr client. Must be using password authentication.
#' @param name human-readable name of the API key to create, e.g. "John's laptop"
#'
#' @return response JSON. The "token" property will contain the API key if successful.
#' @export
create.api.key <- function(gf, name) {
  response.to.JSON(gofigr.POST(gf, "api_key/",
                               body=jsonlite::toJSON(list(name=name),
                                                     auto_unbox=TRUE),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

get.api.id <- function(obj) {
  if(is.character(obj)) {
    return(obj)
  } else if(is.list(obj)) {
    return(obj$api_id)
  } else {
    stop(paste0("Unable to obtain API ID for object ", obj))
  }
}

obj.to.JSON <- function(obj, auto_unbox=TRUE, ...) {
  jsonlite::toJSON(obj, auto_unbox = auto_unbox, ...)
}


null.to.empty <- function(x) {default.if.null(x, "")}

find.or.create <- function(gf, name, get.list, do.create, create=FALSE, type="object") {
  objects <- get.list()
  if(length(objects) > 0) {
    matches <- objects[sapply(objects, function(x) {x$name == name})]
  } else {
    matches <- list() # because R doesn't like empty lists as subscripts
  }

  if(length(matches) == 1) {
    return(matches[[1]])
  } else if(length(matches) > 1) {
    stop(paste0("Multiple instances of ", type, " match the name \"", name, "\". Please use an API ID instead."))
  } else {
    # No matches
    if(!create) {
      stop("Could not find any ", type, " matches for \"", name, "\". Did you mean to specify create=TRUE?")
    }

    return(do.create())
  }
}
