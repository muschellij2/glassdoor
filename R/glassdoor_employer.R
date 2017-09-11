#' Glassdoor Company Search
#'
#' @param ... arguments to pass to \code{\link{gd_api}}
#' @param location Scope the search to a specific location by specifying it
#' here - city, state, or country.
#' @param city	Scope the search to a specific city by specifying it here.
#' @param state	Scope the search to a specific state by specifying it here.
#' @param country	Scope the search to a specific country by specifying it here.
#' @param page_number Page number to retrieve. Default is 1.
#' @param page_size Page size, i.e. the number of jobs returned on
#' each page of results. Default is 20.
#' @param query Additional options to pass to the query other than those
#' specified here
#' @export
#'
#' @examples
#' if (have_gd_tokens()) {
#'
#' paste0("action=employers")
#' res = gd_company()
#' }
#'
gd_company <- function(
  ...,
  location = NULL,
  city = NULL,
  state = NULL,
  country = NULL,
  page_number = NULL,
  page_size = NULL,
  query = NULL
) {

  action = "employers"
  args = list(...)
  args$action = action

  if (!is.null(location)) {
    location = as.character(location)
    location = match.arg(location, c("city", "state", "country"))
  }

  qq = list(
    l = location,
    city = city,
    state = state,
    country = country,
    pn = page_number,
    ps = page_size
  )

  no_null = vapply(qq, function(x) !all(is.null(x)), logical(1))
  qq = qq[no_null]

  qnames = names(query)
  if (length(query) > 0) {
    for (iname in qnames) {
      qq[[iname]] = query[[iname]]
    }
  }

  args$query = qq

  res = do.call("gd_api", args)

  return(res)
}

#' @rdname gd_company
#' @export
gd_employer = gd_company
