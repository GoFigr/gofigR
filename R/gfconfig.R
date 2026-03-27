#' Reads a line from stdin with optional validation and retry logic.
#'
#' This is a small helper used by the GoFigr configuration wizard to collect
#' user input interactively. When a validation function is supplied, the input
#' will be repeatedly requested until it passes validation or the maximum
#' number of attempts is reached.
#'
#' @param prompt Character string shown to the user, e.g. "Enter username: ".
#' @param validate Optional function taking a single character argument and
#'   either returning a transformed value or throwing an error if the value
#'   is invalid.
#' @param attempt Current attempt number (used internally for recursion).
#' @param max_attempts Maximum number of attempts before giving up and
#'   throwing an error.
#'
#' @return The raw input string, or the result of `validate(input)` if
#'   a validation function is supplied.
read_prompt <- function(prompt, validate=NULL, attempt=1, max_attempts=2) {
  if(attempt > max_attempts) {
    stop(paste0("Failed after ", max_attempts, " attempts"))
  }

  res <- readline(prompt)
  if(!is.null(validate)) {
    tryCatch({
      val <- validate(res)
    }, error=function(err) {
      message(paste0(err, "\n"))
      return(read_prompt(prompt, validate=validate, attempt = attempt + 1))
    })
  } else {
    return(res)
  }
}

#' Fetches Auth0 configuration from the server's /info endpoint.
#'
#' @param api_url Base API URL (e.g. "https://api.gofigr.io")
#'
#' @return A list with domain, client_id, and audience, or NULL if unavailable.
get_auth0_config <- function(api_url) {
  info_url <- paste0(api_url, "/api/", API_VERSION, "/info/")
  tryCatch({
    resp <- httr::GET(info_url, httr::timeout(5))
    if (resp$status_code != 200) return(NULL)
    data <- jsonlite::fromJSON(rawToChar(resp$content))
    domain <- data$auth0_domain
    client_id <- data$auth0_cli_client_id
    audience <- default_if_null(data$auth0_audience, "")
    if (is.null(domain) || is.null(client_id) || domain == "" || client_id == "") {
      return(NULL)
    }
    return(list(domain=domain, client_id=client_id, audience=audience))
  }, error=function(err) {
    return(NULL)
  })
}

#' Authenticates via Auth0 Device Authorization Flow.
#'
#' Requests a device code, displays the verification URL, optionally opens
#' the browser, and polls for authorization. Returns a GoFigr client
#' authenticated with the Auth0 access token.
#'
#' @param api_url Base API URL
#' @param auth0_config Auth0 configuration from get_auth0_config()
#' @param max_attempts Not used (kept for API consistency)
#'
#' @return A configured GoFigr client authenticated with an Auth0 token.
login_with_device_flow <- function(api_url, auth0_config, max_attempts=3) {
  device_url <- paste0("https://", auth0_config$domain, "/oauth/device/code")
  resp <- httr::POST(device_url,
                     body=jsonlite::toJSON(list(
                       client_id=auth0_config$client_id,
                       audience=auth0_config$audience,
                       scope="openid email profile"
                     ), auto_unbox=TRUE),
                     httr::content_type_json(),
                     httr::timeout(10))

  if (resp$status_code != 200) {
    data <- tryCatch(jsonlite::fromJSON(rawToChar(resp$content)), error=function(e) list())
    detail <- default_if_null(data$error_description, rawToChar(resp$content))
    stop(paste0("Failed to start device authorization (HTTP ", resp$status_code, "): ", detail))
  }

  data <- jsonlite::fromJSON(rawToChar(resp$content))
  verification_uri <- default_if_null(data$verification_uri_complete, data$verification_uri)
  user_code <- data$user_code
  device_code <- data$device_code
  interval <- default_if_null(data$interval, 5)

  message("\n  To log in, open this URL in your browser:\n")
  message(paste0("    ", verification_uri, "\n"))
  message(paste0("  And enter code: ", user_code, "\n"))

  tryCatch({
    utils::browseURL(verification_uri)
    message("  (Browser opened automatically)\n")
  }, error=function(e) {})

  message("  Waiting for authorization...")

  token_url <- paste0("https://", auth0_config$domain, "/oauth/token")
  while (TRUE) {
    Sys.sleep(interval)
    token_resp <- httr::POST(token_url,
                             body=jsonlite::toJSON(list(
                               grant_type="urn:ietf:params:oauth:grant-type:device_code",
                               client_id=auth0_config$client_id,
                               device_code=device_code
                             ), auto_unbox=TRUE),
                             httr::content_type_json(),
                             httr::timeout(10))

    token_data <- jsonlite::fromJSON(rawToChar(token_resp$content))

    if (token_resp$status_code == 200) {
      message("  => Authenticated successfully\n")
      access_token <- token_data$access_token

      gf <- gofigr_client(url=api_url, anonymous=TRUE, ignore_config=TRUE)
      gf$access_token <- access_token
      return(gf)
    }

    error <- default_if_null(token_data$error, "")
    if (error == "authorization_pending") {
      next
    } else if (error == "slow_down") {
      interval <- interval + 1
      next
    } else if (error == "expired_token") {
      stop("Authorization timed out. Please try again.")
    } else if (error == "access_denied") {
      stop("Authorization was denied.")
    } else {
      detail <- default_if_null(token_data$error_description, error)
      stop(paste0("Device flow error: ", detail))
    }
  }
}

#' Prompts the user for an API key or interactively creates a new one.
#'
#' Given an authenticated GoFigr client, this helper either accepts an
#' existing API key entered by the user or creates a new API key via the API.
#' The newly created key is associated with the authenticated user.
#'
#' @param gf Authenticated GoFigr client created by `gofigr_client()`.
#' @param max_attempts Maximum number of attempts when validating a user-
#'   supplied API key.
#'
#' @return A character string containing a valid API key, either supplied
#'   by the user or newly created.
login_with_api_key <- function(gf, max_attempts) {
  api_key <- read_prompt("Paste an existing API key, or press Enter to generate a new one: ",
                         validate=function(api_key) {
                           api_key <- trimws(api_key)
                           if(api_key == "") {return(api_key)}

                           gf_tmp <- gofigr_client(api_key=api_key,
                                                   ignore_config=TRUE)
                           info_tmp <- user_info(gf_tmp)
                           return(api_key)
                         },
                         max_attempts = max_attempts)
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
                          },
                          max_attempts = max_attempts)

  created_key <- create_api_key(gf, key_name)
  return(created_key$token)
}

#' Interactive configuration helper for the GoFigr R client.
#'
#' Runs a simple text-based wizard that authenticates via Auth0 Device Code
#' flow, generates or verifies an API key, lets the user choose a default
#' workspace, and then writes a configuration file to `~/.gofigr`. This
#' configuration is used by `gofigr_client()` when explicit credentials
#' are not provided.
#'
#' @param url API URL. Default: https://api.gofigr.io
#' @param max_attempts Maximum number of API key attempts before
#'   the wizard aborts with an error.
#'
#' @return Invisibly returns `NULL`. The main effect is writing configuration
#'   to disk and printing progress messages.
#' @export
gfconfig <- function(url=NULL, max_attempts=3) {
  api_url <- default_if_null(url, API_URL)

  message("-------------------------------------------------------------------\n")
  message("Welcome to GoFigr! This wizard will help you get up and running.\n")
  message("-------------------------------------------------------------------\n\n")

  auth0_config <- get_auth0_config(api_url)
  if (is.null(auth0_config)) {
    stop(paste0("Could not fetch Auth0 configuration from ", api_url, "/api/", API_VERSION, "/info/. ",
                "Please check the URL and try again."))
  }

  gf_auth <- login_with_device_flow(api_url, auth0_config, max_attempts)
  api_key <- login_with_api_key(gf_auth, max_attempts)
  gf <- gofigr_client(api_key=api_key, url=api_url)

  message("Fetching workspaces...\n")
  worxs <- list_workspaces(gf)
  worx_df <- NULL
  id <- 1
  lapply(worxs, function(wx) {
    worx_df <<- rbind(worx_df, data.frame(Number=c(id),
                                          Name=c(wx$name),
                                          Description=c(if(is.null(wx$description)) "N/A" else wx$description),
                                          `API ID`=c(wx$api_id)))
    id <<- id + 1
  })

  lapply(1:length(worxs), function(id) {
    wx <- worxs[[id]]
    message(paste0(id, ". ", wx$name, " - ", wx$api_id))
  })

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
                 workspace=worx_id,
                 url=api_url)

  config_path <- CONFIG_PATH
  fileConn <- file(config_path)
  write(jsonlite::toJSON(config, auto_unbox=TRUE, pretty=TRUE), fileConn)
  close(fileConn)

  invisible(message(paste0("\nConfiguration saved to ", config_path, ". Happy analysis!\n")))
}
