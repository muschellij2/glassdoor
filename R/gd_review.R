#' Glassdoor Company Search
#'
#' @param ... arguments to pass to \code{\link{gd_api}}
#' @param employer_id Glassdoor ID for the company, can be accessed using
#' \code{\link{gd_company}}
#' @param page_number Page number to retrieve. Default is 1.
#' @param page_size Page size, i.e. the number of jobs returned on
#' each page of results. Default is 20.
#' @param query Additional options to pass to the query other than those
#' specified here
#' @export
#'
#' @examples
#' if (have_gd_tokens()) {
#' res = gd_review(employer_id = 715) # walmart
#' }
#'
gd_review <- function(
  employer_id,
  ...,
  page_number = NULL,
  page_size = NULL,
  query = NULL
) {

  # action = "reviews"
  action = "employer-overview"
  args = list(...)
  args$action = action


  qq = list()
  qq$pn = page_number
  qq$ps = page_size
  qq$employerId = employer_id


  no_null = vapply(qq, function(x) {
    !all(is.null(x))
    }, logical(1))
  qq = qq[no_null]

  qnames = names(query)
  if (length(query) > 0) {
    for (iname in qnames) {
      qq[[iname]] = query[[iname]]
    }
  }

  args$add_query = qq
  res = do.call("gd_api", args)

  return(res)
}
