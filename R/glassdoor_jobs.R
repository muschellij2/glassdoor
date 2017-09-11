#' Glassdoor Job Stats
#'
#' @param ... arguments to pass to \code{\link{glassdoor_api}}
#' @param employer	Scope the search to a specific employer by
#' specifying the name here.
#' @param location	Scope the search to a specific location by specifying it
#' here - city, state, or country.
#' @param city	Scope the search to a specific city by specifying it here.
#' @param state	Scope the search to a specific state by specifying it here.
#' @param country	Scope the search to a specific country by specifying it here.
#' @param fromAge	Scope the search to jobs that are less than X days old
#' (-1 = show all jobs (default), 1 = 1 day old, 7 = 1 week old, 14 = 2 weeks
#' old, etc.)
#' @param jobType	Scope the search to certain job types. Valid values are all
#' (default), fulltime, parttime, internship, contract, internship, temporary
#' @param minRating	Scope the search to jobs of companies
#' with rating >= minRating
#' (0 = returns all (default), 1 = more than 1 star, 2 = more than 2 stars,
#' 3 = more than 3 stars, 4 = more than 4 stars)
#' @param radius	Scope the search to jobs within a certain radius, in miles,
#' of the location specified.
#' @param job_title Scope the search to a specific job title by specifying
#' it here.
#' @param job_category Job category id to scope the search to - see the Job
#' Category table below - note you must pass the id. This can be a comma
#' separated list of ids if you desire to select more than one category.
#' @param returnCities	Results will include geographical data (job counts)
#' broken down by city.
#' @param returnStates	Results will include geographical data (job counts,
#' score) broken down by the type of geographical district specified in
#' parameter admLevelRequested.
#' @param returnJobTitles	Results will include job data broken down by job
#' title.
#' @param returnEmployers	Results will include job data broken down by
#' employer.
#' @param admLevelRequested	Geographic district type requested when
#' returnStates is true (1 = states, 2 = counties)
#' @param query Additional options to pass to the query other than those
#' specified here
#' @export
#' @examples
#' if (have_glassdoor_tokens()) {
#'
#' paste0("returnStates=true&admLevelRequested=1")
#' res = glassdoor_job_stats(returnStates = TRUE, admLevelRequested = 1)
#' }
glassdoor_job_stats <- function(
  ...,
  employer = NULL,
  location = NULL,
  city = NULL,
  state = NULL,
  country = NULL,
  fromAge = NULL,
  jobType = NULL,
  minRating = NULL,
  radius = NULL,
  job_title = NULL,
  job_category = NULL,
  returnCities = NULL,
  returnStates = NULL,
  returnJobTitles = NULL,
  returnEmployers = NULL,
  admLevelRequested = NULL,
  query = NULL
) {

  action = "jobs-stats"
  args = list(...)
  args$action = action

  if (!is.null(admLevelRequested)) {
    admLevelRequested = as.character(admLevelRequested)
    admLevelRequested = match.arg(admLevelRequested, choices = c(1, 2))
  }

  if (!is.null(minRating)) {
    minRating = as.character(minRating)
    minRating = match.arg(minRating, choices = 0:4)
  }

  make_true_false = function(x) {
    if (is.null(x)) {
      return(NULL)
    }
    x = as.logical(x)
    x = as.character(x)
    x = tolower(x)
    return(x)
  }

  if (!is.null(location)) {
    location = as.character(location)
    location = match.arg(location, c("city", "state", "country"))
  }


  qq = list(
    e = employer,
    l = location,
    city = city,
    state = state,
    country = country,
    fromAge = fromAge,
    jobType = jobType,
    minRating = minRating,
    radius = radius,
    # need renaming
    jt = job_title,
    jc = job_category,
    returnCities = returnCities,
    returnStates = returnStates,
    returnJobTitles = returnJobTitles,
    returnEmployers = returnEmployers,
    admLevelRequested = admLevelRequested
  )

  logicals = c("returnCities", "returnStates", "returnJobTitles",
               "returnEmployers")
  qq[logicals] = lapply(qq[logicals], make_true_false)

  no_null = vapply(qq, function(x) !all(is.null(x)), logical(1))
  qq = qq[no_null]

  qnames = names(query)
  if (length(query) > 0) {
    for (iname in qnames) {
      qq[[iname]] = query[[iname]]
    }
  }

  args$query = qq

  res = do.call("glassdoor_api", args)

  return(res)
}