#' Glassdoor Job Progression
#'
#' @param ... arguments to pass to \code{\link{gd_api}}
#' @param jobTitle Job Title - the job title to get related jobs for
#' @param countryId	Country Id - only 1 (US) is supported right now.
#' @param query Additional options to pass to the query other than those
#' specified here
#' @export
#'
#' @examples
#' if (have_gd_tokens()) {
#'
#' paste0("&countryId=1&jobTitle=cashier")
#' res = gd_job_prog(jobTitle = "cashier", countryId = 1)
#' }
#'
gd_job_prog <- function(
  ...,
  jobTitle,
  countryId = 1,
  query = NULL
) {

  action = "jobs-prog"
  args = list(...)
  args$action = action

  countryId = as.character(countryId)


  qq = list(
    jobTitle = jobTitle,
    countryId = countryId
  )

  # no_null = vapply(qq, function(x) !all(is.null(x)), logical(1))
  # qq = qq[no_null]

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