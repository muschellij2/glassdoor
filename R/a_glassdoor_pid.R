#' Glassdoor Partner ID
#'
#' @param token Partner ID or Authentication token
#' @param error Should the function error if no token specified?
#'
#' @return A vector of class character
#' @export
#'
#' @examples
#' glassdoor_pid()
glassdoor_pid <- function(token = NULL, error = TRUE){
  token = glassdoor_token(
    token = token, error = error,
    varname = "GLASSDOOR_PID")
  return(token)
}

#' @rdname glassdoor_pid
#' @export
glassdoor_pat <- function(token = NULL, error = TRUE){
  token = glassdoor_token(
    token = token, error = error,
    varname = "GLASSDOOR_PAT")
  return(token)
}


#' @rdname glassdoor_pid
#' @export
have_glassdoor_pid <- function(token = NULL){
  token = glassdoor_pid(
    token = token,
    error = FALSE)
  return(!is.null(token))
}

#' @rdname glassdoor_pid
#' @export
have_glassdoor_pat <- function(token = NULL){
  token = glassdoor_pat(
    token = token,
    error = FALSE)
  return(!is.null(token))
}


glassdoor_token <- function(token = NULL,
                            error = TRUE,
                            varname = "GLASSDOOR_PID"){
  if (is.null(token)) {
    token = Sys.getenv(varname)
  }
  if (token == "") {
    token = NULL
  }
  if (is.null(token)) {
    if (error) {
      stop(paste0("Token: ", varname, " not defined!"))
    }
  }
  return(token)
}

