#' Glassdoor Partner ID
#'
#' @param token Partner ID or Authentication token
#' @param error Should the function error if no token specified?
#'
#' @return A vector of class character
#' @export
#'
#' @examples
#' if (have_gd_pid()) {
#' gd_pid()
#' }
gd_pid <- function(token = NULL, error = TRUE){
  token = gd_token(
    token = token, error = error,
    varname = "GLASSDOOR_PID")
  return(token)
}

#' @rdname gd_pid
#' @export
gd_pat <- function(token = NULL, error = TRUE){
  token = gd_token(
    token = token, error = error,
    varname = "GLASSDOOR_PAT")
  return(token)
}


#' @rdname gd_pid
#' @export
have_gd_pid <- function(token = NULL){
  token = gd_pid(
    token = token,
    error = FALSE)
  return(!is.null(token))
}

#' @rdname gd_pid
#' @export
have_gd_pat <- function(token = NULL){
  token = gd_pat(
    token = token,
    error = FALSE)
  return(!is.null(token))
}

#' @rdname gd_pid
#' @export
have_gd_tokens <- function() {
  have_gd_pat() && have_gd_pid()
}


gd_token <- function(token = NULL,
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

