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

#' Prompts the user for username and password and logs into GoFigr.
#'
#' This function interactively requests credentials, attempts authentication
#' against the GoFigr API, and retries a limited number of times on failure.
#' It is primarily used by `gfconfig()` and is not intended for scripted use.
#'
#' @param max_attempts Maximum number of login attempts before giving up.
#'
#' @return A configured GoFigr client object authenticated with username
#'   and password.
login_with_username <- function(max_attempts) {
  connection_ok <- FALSE
  attempt <- 0
  while(!connection_ok && attempt < max_attempts) {
    username <- read_prompt("Username: ")
    password <- getPass::getPass("Password: ")

    message("Testing connection...\n")

    tryCatch({
      gf <- gofigr_client(username=username,
                          password=password,
                          ignore_config=TRUE)
      info <- user_info(gf) # Make an authenticated request

      if(!is.null(info$username)) {
        message("  => Success\n")
        connection_ok <- TRUE
      } else {
        stop("Unknown error occurred.")
      }
    }, error=function(err) {
      message(paste0(err, "\n"))
      message("Connection failed. Please verify your username & password and try again.\n\n")
      attempt <<- attempt + 1
    })
  }

  if(!connection_ok) {
    stop(paste0("Connection failed after ", max_attempts, " attempts"))
  }

  return(gf)
}

#' Prompts the user for an API key or interactively creates a new one.
#'
#' Given a password-authenticated GoFigr client, this helper either accepts an
#' existing API key entered by the user or creates a new API key via the API.
#' The newly created key is associated with the authenticated user.
#'
#' @param gf Password-authenticated GoFigr client created by `gofigr_client()`.
#' @param max_attempts Maximum number of attempts when validating a user-
#'   supplied API key.
#'
#' @return A character string containing a valid API key, either supplied
#'   by the user or newly created.
login_with_api_key <- function(gf, max_attempts) {
  api_key <- read_prompt("API key (leave blank to generate a new one): ",
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
#' Runs a simple text-based wizard that logs into GoFigr, generates or verifies
#' an API key, lets the user choose a default workspace, and then writes a
#' configuration file to `~/.gofigr`. This configuration is used by
#' `gofigr_client()` when explicit credentials are not provided.
#'
#' @param max_attempts Maximum number of password/API key attempts before
#'   the wizard aborts with an error.
#'
#' @return Invisibly returns `NULL`. The main effect is writing configuration
#'   to disk and printing progress messages.
#' @export
gfconfig <- function(max_attempts=3) {
  message("-------------------------------------------------------------------\n")
  message("Welcome to GoFigr! This wizard will help you get up and running.\n")
  message("-------------------------------------------------------------------\n\n")

  gf_pw_auth <- login_with_username(max_attempts)
  api_key <- login_with_api_key(gf_pw_auth, max_attempts)
  gf <- gofigr_client(api_key=api_key)

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
                 workspace=worx_id)

  config_path <- CONFIG_PATH
  fileConn <- file(config_path)
  write(jsonlite::toJSON(config, auto_unbox=TRUE, pretty=TRUE), fileConn)
  close(fileConn)

  invisible(message(paste0("\nConfiguration saved to ", config_path, ". Happy analysis!\n")))
}
