#' General Glassdoor API function
#'
#' @param action The particular API call that you would like to make
#' @param query Additional options to pass to the query other than those
#' specified here
#' @param agent The User-Agent (browser) of the end user to whom the API
#' results will be shown. Note that you can can obtain this from the
#' "User-Agent" HTTP request header from the end-user
#' @param version The API version. The current version is 1 except for
#' jobs, which is currently version 1.1
#' @param format Either \code{xml} or \code{json} as you prefer
#' @param url Glassdoor RESTful API URL
#' @param pid Your partner id, as assigned by Glassdoor
#' @param pat Your partner key, as assigned by Glassdoor
#' @param ip_address The IP address of the end user to whom the API results will be shown
#' @param add_query Additional options to pass to the query other than those
#' specified here (named list)
#' @param ... Additional options to send to \code{\link{GET}}
#'
#' @return A list of class \code{gd_api}
#' @export
#'
#' @examples
#' if (have_gd_pid() && have_gd_pat()) {
#' res = gd_api(
#' action = "employers",
#' other = NULL,
#'   version = 1,
#' format = "json",
#' query =  "pharmaceuticals")
#'
#' res = gd_api(
#' action = "employers",
#' other = NULL,
#'  version = 1,
#' format = "json",
#' query = "pharmaceuticals", config = list())
#' }
#' @importFrom httr content GET
gd_api <- function(
  query = NULL,
  action = NULL,
  agent = gd_user_agent(),
  version = 1,
  format = "json",
  url = gd_url(),
  pid = gd_pid(),
  pat = gd_pat(),
  ip_address = NULL,
  add_query = NULL,
  ...
) {

  if (is.null(ip_address)) {
    ip_address = get_ip(agent = agent)
  }

  # ua = httr::user_agent(agent)
  # ua = paste
  qq = list()
  qq$v = version
  qq$format = format
  qq$t.p = pid
  qq$t.k = pat
  qq$action = action
  qq$userip = ip_address
  qq$useragent = agent
  qq$q = query


  qnames = names(add_query)
  if (length(add_query) > 0) {
    for (iname in qnames) {
      qq[[iname]] = add_query[[iname]]
    }
  }
  # qq = c(qq, query)

  res = httr::GET(url, query = qq, ...)

  #
  exported = structure(list(
    content = httr::content(res),
    query = qq,
    response = res
  ), class = "gd_api")

  httr::warn_for_status(exported$response)
  gd_success = as.logical(exported$content$success)
  if (is.null(gd_success)) {
    if (!gd_success) {
      warning("Glassdoor did not indicate successful retrieval!")
    }
  }
  return(exported)
}
