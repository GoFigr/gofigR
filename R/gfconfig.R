#' Reads a prompt from stdin and performs optional validation
#'
#' @param prompt prompt, e.g. "Enter username: "
#' @param validate function input => transformed input if input is valid or, error
#' @param attempt current attempt at getting a valid input
#' @param max_attempts maximum number of attempts
#'
#' @return input, or result of validate(input) if validate is supplied
#'
#' @examples
#' read.prompt("Enter username: ", validate=trimws)
read.prompt <- function(prompt, validate=NULL, attempt=1, max_attempts=2) {
  if(attempt > max_attempts) {
    stop(paste0("Failed after ", max_attempts, " attempts"))
  }

  res <- readline(prompt)
  if(!is.null(validate)) {
    tryCatch({
      val <- validate(res)
    }, error=function(err) {
      cat(paste0(err, "\n"))
      return(read.prompt(prompt, validate=validate, attempt = attempt + 1))
    })
  } else {
    return(res)
  }
}

#' Prompts the user for username & password and logs into GoFigr
#'
#' @param max_attempts
#'
#' @return GoFigr client
login.with.username <- function(max_attempts) {
  connection_ok <- FALSE
  attempt <- 0
  while(!connection_ok && attempt < max_attempts) {
    username <- read.prompt("Username: ")
    password <- read.prompt("Password: ")

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

#' Prompts the user for an API key or creates a new one
#'
#' @param gf Password-authenticated GoFigr client
#' @param max_attempts
#'
#' @return API key, either supplied by the user or newly created
login.with.api.key <- function(gf, max_attempts) {
  api_key <- read.prompt("API key (leave blank to generate a new one): ",
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

  key_name <- read.prompt("Key name (e.g. Alyssa's laptop): ",
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

#' Configures gofigr for use on this machine. Saves configuration
#' to ~/.gofigr.
#'
#' @param max_attempts Maximum number of password attempts
#'
#' @return No return value
#' @export
#'
#' @examples
#' gfconfig()
gfconfig <- function(max_attempts=3) {
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
  worx_id <- read.prompt(paste0("\nPlease select a default workspace (", range, "): "),
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
  write(toJSON(config, auto_unbox=TRUE, pretty=TRUE), fileConn)
  close(fileConn)

  invisible(cat(paste0("\nConfiguration saved to ", config_path, ". Happy analysis!")))
}
