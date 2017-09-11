#' General Glassdoor API function
#'
#' @param action The particular API call that you would like to make
#' @param other Each API action will require different parameters
#' For example, an employerId is passed in order to retrieve reviews.
#' @param agent The User-Agent (browser) of the end user to whom the API
#' results will be shown. Note that you can can obtain this from the
#' "User-Agent" HTTP request header from the end-user
#' @param q Query parameter (not a list) for Glassdoor
#' @param version The API version. The current version is 1 except for
#' jobs, which is currently version 1.1
#' @param format Either \code{xml} or \code{json} as you prefer
#' @param url Glassdoor RESTful API URL
#' @param pid Your partner id, as assigned by Glassdoor
#' @param pat Your partner key, as assigned by Glassdoor
#' @param ip_address The IP address of the end user to whom the API results will be shown
#' @param query Additional options to pass to the query other than those
#' specified here
#' @param ... Additional options to send to \code{\link{GET}}
#'
#' @return A list of class \code{glassdoor_api}
#' @export
#'
#' @examples
#' if (have_glassdoor_pid() && have_glassdoor_pat()) {
#' res = glassdoor_api(
#' action = "employers",
#' other = NULL,
#'   version = 1,
#' format = "json",
#' query = list(q = "pharmaceuticals"))
#'
#' res = glassdoor_api(
#' action = "employers",
#' other = NULL,
#'   version = 1,
#' format = "json",
#' q = "pharmaceuticals", config = list())
#' }
#' @importFrom httr content GET
glassdoor_api <- function(
  action = NULL,
  other = NULL,
  agent = glassdoor_user_agent(),
  version = 1,
  q = NULL,
  format = "json",
  url = glassdoor_url(),
  pid = glassdoor_pid(),
  pat = glassdoor_pat(),
  ip_address = NULL,
  query = NULL,
  ...
) {

  if (is.null(ip_address)) {
    ip_address = get_ip(agent = agent)
  }

  # ua = httr::user_agent(agent)
  # ua = paste

  qq = list(
    v = version,
    format = format,
    t.p = pid,
    t.k = pat,
    action = action,
    other = other,
    userip = ip_address,
    useragent = agent
  )
  if (!is.null(other)) {
    qq$other = other
  }
  qq$q = q


  qnames = names(query)
  if (length(query) > 0) {
    for (iname in qnames) {
      qq[[iname]] = query[[iname]]
    }
  }
  # qq = c(qq, query)

  res = httr::GET(url, query = qq, ...)
  #
  exported = structure(list(
    content = httr::content(res),
    query = qq,
    response = res
  ),
  class = "glassdoor_api")
  return(exported)
}
