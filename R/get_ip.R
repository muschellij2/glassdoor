
#' Get IP Address
#'
#' @param agent User Agent for Header
#'
#' @return Character Vector of IP
#' @export
#' @note Inspired by
#' \url{https://github.com/gregce/ipify/blob/master/R/ipify.R}
#' @export
#' @examples
#' get_ip()
get_ip = function(agent = gd_user_agent()) {
  url = "https://api.ipify.org"
  ip_address = httr::GET(url = url, httr::user_agent(agent))
                         # query = list(format = json"))
  ip_address = httr::content(ip_address, encoding = "UTF-8")
}