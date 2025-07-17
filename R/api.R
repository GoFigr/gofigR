API_URL = "https://api.gofigr.io"
API_VERSION = "v1.2"

APP_URL = "https://app.gofigr.io"

#' Default path to the config file
#'
#' @return file path
#' @export
CONFIG_PATH = file.path(path.expand('~'), ".gofigr")

#' Finds the .gofigr config file in current directory or any of the
#' parent directories. If the file cannot be found, will also check
#' CONFIG_PATH.
#'
#' @param start_dir top-level directory where to start looking. getwd()
#'  by default.
#'
#' @returns path to .gofigr, or NULL if not found
#' @export
find_config <- function(start_dir=NULL) {
  if(is.null(start_dir)) {
    start_dir <- getwd()
  }

  current <- file.path(start_dir, ".gofigr")
  if(file.exists(current)) {
    return(current)
  } else if(dirname(start_dir) != start_dir) {
    return(find_config(dirname(start_dir)))
  } else if (!is.null(CONFIG_PATH) && file.exists(CONFIG_PATH)) {
    return(CONFIG_PATH)
  } else {
    return(NULL)
  }
}

#' Reads the GoFigr configuration, prioritizing environment variables over the
#' config file:
#'
#' * GF_USERNAME or config["username"]
#' * GF_PASSWORD or config["password"]
#' * GF_API_KEY or config["api_key"]
#' * GF_WORKSPACE or config["workspace"]
#' * GF_URL or config["url"]
#'
#' @param path path to the config file, default find_config()
#'
#' @return parsed configuration or empty list if not available
#' @export
read_config <- function(path=NULL) {
  if(is.null(path)) {
    path <- find_config()
  }

  if(is.null(path) || !file.exists(path)) {
    return(list())
  }

  tryCatch({
    data <- jsonlite::fromJSON(file(path))

    # Prioritize environment variables
    data$username <- Sys.getenv("GF_USERNAME", unset=default_if_null(data$username, ""))
    data$password <- Sys.getenv("GF_PASSWORD", unset=default_if_null(data$password, ""))
    data$api_key <- Sys.getenv("GF_API_KEY", unset=default_if_null(data$api_key, ""))
    data$workspace <- Sys.getenv("GF_WORKSPACE", unset=default_if_null(data$workspace, ""))
    data$url <- Sys.getenv("GF_URL", unset=default_if_null(data$url, API_URL))

  return(data)
  }, error=function(err) {
    warning(paste0("WARNING: Configuration ", path, " cannot be read: ", err, "\n"))
    return(list())
  })
}

#' Returns a default value if argument is null or empty
#'
#' @param x argument
#' @param default default value if x is null, NA or ""
#'
#' @return x if not null, NA or "", or the default value
#' @export
default_if_null <- function(x, default) {
  if(is.null(x) || identical(x, NA) || identical(x, "") || identical(x, character(0))) {
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
#' @param ignore_config if TRUE, will ignore environment variables and other
#' external configuration
#'
#' @return configured GoFigr client which you can pass to other functions
#' @export
#'
#' @examples
#' \dontrun{gofigr_client()  # use config from ~/.gofigr or environment variables}
#' \dontrun{gofigr_client(username="joe", password="abc123") # password login}
#' \dontrun{gofigr_client(api_key="abcdef0123456789") # API key login}
gofigr_client <- function(username=NULL, password=NULL, api_key=NULL,
                  url=NULL, anonymous=FALSE, verbose=FALSE,
                  workspace=NULL, ignore_config=FALSE) {
  config <- if(ignore_config) list() else read_config()

  api_url <- default_if_null(url, config$url)
  if(is.null(api_url)) {
    api_url <- API_URL
  }

  client <- structure(
    local({username=default_if_null(username, config$username)
           password=default_if_null(password, config$password)
           api_key=default_if_null(api_key, config$api_key)
           base_url=paste0(api_url)
           url=paste0(api_url, "/api/", API_VERSION, "/")
           jwt_url=paste0(api_url, "/api/token/")
           anonymous=anonymous
           access_token=NULL
           refresh_token=NULL
           verbose=verbose
           workspace=default_if_null(workspace, config$workspace)
           environment()
                 }))

  class(client) <- "gofigr"
  return(client)
}

#' Default print method for a GoFigr client.
#'
#' @param x GoFigr client
#' @param ... passed to cat
#'
#' @return NA
#' @export
print.gofigr <- function(x, ...) {
  cat(paste0("GoFigr client at ", x$url, "\n"), ...)
}

#' Equivalent to cat but only outputs if GoFigr client is verbose.
#'
#' @param gf GoFigr client
#' @param content text to print
#' @param ... passed to cat
#'
#' @return NA
gofigr_cat <- function(gf, content, ...) {
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

  gofigr_cat(gf, "JWT authentication successful")
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

    gofigr_cat(gf, "JWT refresh successful")
  } else {
    gofigr_cat(gf, "JWT refresh failed. Attempting re-authentication.")
    authenticate_jwt(gf)
  }
}

#' Returns True if the response indicates an expired JWT token
#'
#' @param res httr response
#'
#' @return True if token expired
is_expired_token <- function(res) {
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
response_to_JSON <- function(response) {
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
gofigr_make_handler <- function(name, method) {
  function(gf, url, expected_status_code=200, ...) {
    full_url <- paste0(gf$url, url)
    gofigr_cat(gf, paste0(name, ": ", full_url))

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

      if(is_expired_token(res)) {  # Token expired?
        gofigr_cat(gf, "Token expired. Trying refresh.")
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

#' Wrapper for httr::GET that automatically handles authentication.
#' @param gf configured GoFigr client
#' @param url URL to make the request to, relative to the API URL e.g. user/
#' @param expected_status_code expected HTTP response code. We will throw
#'  an exception if it differs.
#' @param ... passed to the httr request function
#' @return result of calling the underlying httr request function
#' @export
gofigr_GET <- gofigr_make_handler("GET", httr::GET)

#' Wrapper for httr::POST that automatically handles authentication.
#' @param gf configured GoFigr client
#' @param url URL to make the request to, relative to the API URL e.g. user/
#' @param expected_status_code expected HTTP response code. We will throw
#'  an exception if it differs.
#' @param ... passed to the httr request function
#' @return result of calling the underlying httr request function
#' @export
gofigr_POST <- gofigr_make_handler("POST", httr::POST)

#' Wrapper for httr::PUT that automatically handles authentication.
#' @param gf configured GoFigr client
#' @param url URL to make the request to, relative to the API URL e.g. user/
#' @param expected_status_code expected HTTP response code. We will throw
#'  an exception if it differs.
#' @param ... passed to the httr request function
#' @return result of calling the underlying httr request function
#' @export
gofigr_PUT <- gofigr_make_handler("PUT", httr::PUT)

#' Wrapper for httr::PATCH that automatically handles authentication.
#' @param gf configured GoFigr client
#' @param url URL to make the request to, relative to the API URL e.g. user/
#' @param expected_status_code expected HTTP response code. We will throw
#'  an exception if it differs.
#' @param ... passed to the httr request function
#' @return result of calling the underlying httr request function
#' @export
gofigr_PATCH <- gofigr_make_handler("PATCH", httr::PATCH)

#' Wrapper for httr::DELETE that automatically handles authentication.
#' @param gf configured GoFigr client
#' @param url URL to make the request to, relative to the API URL e.g. user/
#' @param expected_status_code expected HTTP response code. We will throw
#'  an exception if it differs.
#' @param ... passed to the httr request function
#' @return result of calling the underlying httr request function
#' @export
gofigr_DELETE <- gofigr_make_handler("DELETE", httr::DELETE)


#' Fetches user details for the currently logged in user.
#'
#' @param gf GoFigr client
#'
#' @return user details
#' @export
user_info <- function(gf) {
  response_to_JSON(gofigr_GET(gf, "user/"))[[1]]
}

#' Creates a new API key. This function will only succeed if using password
#' authentication.
#'
#' @param gf GoFigr client. Must be using password authentication.
#' @param name human-readable name of the API key to create, e.g. "John's laptop"
#'
#' @return response JSON. The "token" property will contain the API key if successful.
#' @export
create_api_key <- function(gf, name) {
  response_to_JSON(gofigr_POST(gf, "api_key/",
                               body=jsonlite::toJSON(list(name=name),
                                                     auto_unbox=TRUE),
                               httr::content_type_json(),
                               expected_status_code = 201))
}

#' Returns obj$api_id if argument is an object, or identity if it's a string.
#'
#' @param obj object for which to get the API ID
#'
#' @return API ID, a string
#' @export
get_api_id <- function(obj) {
  if(is.character(obj)) {
    return(obj)
  } else if(is.list(obj)) {
    return(obj$api_id)
  } else {
    stop(paste0("Unable to obtain API ID for object ", obj))
  }
}

obj_to_JSON <- function(obj, auto_unbox=TRUE, ...) {
  jsonlite::toJSON(obj, auto_unbox = auto_unbox, ...)
}


null_to_empty <- function(x) {default_if_null(x, "")}

find_or_create <- function(gf, name, get_list, do_create, create=FALSE, type="object") {
  objects <- get_list()
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

    return(do_create())
  }
}
