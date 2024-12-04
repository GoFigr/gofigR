library(httr)
library(jsonlite)
library(knitr)

API_URL = "http://localhost:8000"
API_VERSION = "v1.2"

APP_URL = "https://app.gofigr.io"

gofigr.client <- function(username=NULL, password=NULL, api_key=NULL,
                  url=API_URL, anonymous=FALSE, verbose=FALSE,
                  workspace=NULL) {
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
           workspace=workspace
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

responseJSON <- function(response) {
  return(fromJSON(rawToChar(response$content),
                  simplifyDataFrame = FALSE,
                  simplifyMatrix = FALSE,
                  simplifyVector = FALSE))
}

gofigr.make_handler <- function(name, method) {
  function(gf, url, expected_status_code=200, ...) {
    full_url <- paste0(gf$url, url)
    gofigr.cat(gf, paste0(name, ": ", full_url))

    if(gf$anonymous) {
      res <- method(full_url)
    } else if(!is.null(gf$api_key)) {
      # API key auth
      res <- method(full_url,
                    add_headers(Authorization = paste0('Token ', gf$api_key)),
                    ...)
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
      }

    }

    if(res$status_code != expected_status_code) {
      stop(paste0("Request to ", full_url, " returned ", res$status_code,
                  ": ",
                  rawToChar(res$content)))
    }

    return(res)
  }
}

gofigr.GET <- gofigr.make_handler("GET", httr::GET)
gofigr.POST <- gofigr.make_handler("POST", httr::POST)
gofigr.PUT <- gofigr.make_handler("PUT", httr::PUT)
gofigr.DELETE <- gofigr.make_handler("DELETE", httr::DELETE)

list.workspaces <- function(gf) {
  responseJSON(gofigr.GET(gf, "workspace/"))
}

get.workspace <- function(gf, api_id) {
  responseJSON(gofigr.GET(gf, paste0("workspace/", api_id)))
}

list.analyses <- function(gf, workspace_id=NULL) {
  worx <- get.workspace(gf, workspace_id || gf.workspace)
  return(worx$analyses)
}

user.info <- function(gf) {
  responseJSON(gofigr.GET(gf, "user/"))[[1]]
}

read_prompt <- function(prompt, validate=NULL, attempt=1, max_attempts=2) {
  if(attempt > max_attempts) {
    stop(paste0("Failed after ", max_attempts, " attempts"))
  }

  res <- readline(prompt)
  if(!is.null(validate)) {
    tryCatch({
      val <- validate(res)
    }, error=function(err) {
      cat(paste0(err, "\n"))
      return(read_prompt(prompt, validate=validate, attempt = attempt + 1))
    })
  } else {
    return(res)
  }
}

login.with.username <- function(max_attempts) {
  connection_ok <- FALSE
  attempt <- 0
  while(!connection_ok && attempt < max_attempts) {
    username <- read_prompt("Username: ")
    password <- read_prompt("Password: ")

    cat("Testing connection...\n")

    tryCatch({
      gf <- gofigr.client(username=username,
                          password=password)
      info <- user.info(gf) # Make an authenticated request

      if(!is.null(info$username)) {
        cat("  => Success\n")
        connection_ok <- TRUE
      } else {
        stop("Unknown error occurred.")
      }
    }, error=function(err) {
      cat(paste0(err, "\n"))
      cat("Connection failed. Please verify your username & password\
          and try again.\n\n")
      attempt <<- attempt + 1
    })
  }

  if(!connection_ok) {
    stop(paste0("Connection failed after ", max_attempts, " attempts"))
  }

  return(gf)
}

create.api.key <- function(gf, name) {
  responseJSON(gofigr.POST(gf, "api_key/",
              body=jsonlite::toJSON(list(name=name),
                                    auto_unbox=TRUE),
              content_type_json(),
              expected_status_code = 201))
}

login.with.api.key <- function(gf, max_attempts) {
  api_key <- read_prompt("API key (leave blank to generate a new one): ",
                         validate=function(api_key) {
                           api_key <- trimws(api_key)
                           if(api_key == "") {return(api_key)}

                           gf_tmp <- gofigr.client(api_key=api_key, verbose=TRUE)
                           info_tmp <- user.info(gf_tmp)
                           return(api_key)
                         })
  if(api_key != "") {
    return(api_key)
  }

  key_name <- read_prompt("Key name (e.g. Alyssa's laptop): ",
                          validate=function(x) {
                            x <- trimws(x)
                            if(x == "") {
                              stop("Name cannot be empty")
                            }
                            return(x)
                          })

  created_key <- create.api.key(gf, key_name)
  return(created_key$token)
}

gfconfig <- function(max_attempts=2) {
  cat("-------------------------------------------------------------------\n")
  cat("Welcome to GoFigr! This wizard will help you get up and running.\n")
  cat("-------------------------------------------------------------------\n\n")

  gf_pw_auth <- login.with.username(max_attempts)
  api_key <- login.with.api.key(gf_pw_auth, max_attempts)
  gf <- gofigr.client(api_key=api_key)

  cat("Fetching workspaces...\n")
  worxs <- list.workspaces(gf)
  worx_df <- NULL
  id <- 1
  lapply(worxs, function(wx) {
    worx_df <<- rbind(worx_df, data.frame(Number=c(id),
                                          Name=c(wx$name),
                                          Description=c(if(is.null(wx$description)) "N/A" else wx$description),
                                          `API ID`=c(wx$api_id)))
    id <<- id + 1
  })

  print(knitr::kable(worx_df))

  range <- paste0(1, "-", max(worx_df$Number))
  worx_id <- read_prompt(paste0("\nPlease select a default workspace (", range, "): "),
                           validate=function(x) {
                             x <- as.numeric(x)
                             if(is.na(x) || x < 1 || x > max(worx_df$Number)) {
                               stop(paste0("Please select a number ", range))
                             }
                             return(worx_df[worx_df$Number == x,]$`API.ID`[1])
                           })

  config <- list(api_key=api_key,
                 workspace=worx_id)

  config_path <- file.path(path.expand('~'), ".gofigr")
  fileConn <- file(config_path)
  write(toJSON(config, auto_unbox=TRUE, pretty=TRUE), fileConn)
  close(fileConn)

  cat(paste0("\nConfiguration saved to ", config_path, ". Happy analysis!"))
}
