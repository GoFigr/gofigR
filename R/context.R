get_rstudio_file_path <- function() {
  tryCatch({
    if(interactive() && rstudioapi::isAvailable()) {
      return(rstudioapi::getSourceEditorContext()$path)
    } else {
      return(NULL)
    }
  }, error=function(err) {
    return(NULL)
  })
}

infer_input_path <- function() {
  if(interactive()) {
    return(rstudioapi::getSourceEditorContext()$path)
  } else {
    return(knitr::current_input(TRUE))
  }
}

get_interactive_context <- function() {
  histfile <- tempfile()
  utils::savehistory(histfile)

  return(list(input_path=histfile,
              chunk_code=NULL,
              metadata=list(context="interactive")))
}

#' Gets the execution context: input path, chunk code, and other metadata.
#'
#' @returns named list with the execution context.
#' @export
#'
#' @examples
#' get_execution_context()
get_execution_context <- function() {
  if(!is.null(knitr::current_input())) {
    # Running in knitr
    options <- knitr::opts_current$get()
    return(list(input_path=knitr::current_input(),
                chunk_code=paste0(options$code, collapse="\n"),
                metadata=list(context='knitr',
                              knitr_options=options)))
  } else if(!interactive()) {
    # Running in a script
    return(list(input_path=scriptName::current_filename(),
                chunk_code=NULL,
                metadata=list(context='script')))
  } else if(interactive() && rstudioapi::isAvailable()) {
    # Running interactively in RStudio
    list(input_path=rstudioapi::getSourceEditorContext()$path,
         input_contents=paste0(rstudioapi::getSourceEditorContext()$contents, collapse="\n"),
         chunk_code=NULL,
         metadata=list(context='RStudio'))
  } else if(interactive() && !rstudioapi::isAvailable()) {
    # Running interactive outside of RStudio
    return(get_interactive_context())
  } else {
    warning("GoFigr could not detect the execution context. Please contact support@gofigr.io.")
    return(list())
  }
}
