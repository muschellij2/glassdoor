#' @rdname results
#' @title Results of a Glassdoor object
#' @description Calculates the results from Glassdoor object
#'
#' @param gd an object for which we want the results, the output
#' from a Glassdoor call
#' @param \dots Any additional arguments to be passed to \code{results}.
#' @export
#' @return A class of \code{gd_job_prog} will return a
#' list of \code{progression_table} and \code{job_info}.
#' @examples
#' if (have_gd_tokens()) {
#'    gd = gd_company("walmart")
#'    res = results(gd)
#' }
results = function(gd, ...){
  UseMethod("results")
}

#' @rdname results
#' @export
results.default = function(gd, ...){
  return(gd)
}

#' @rdname results
#' @export
results.gd_api = function(gd, ...){
  NextMethod()
}


#' @rdname results
#' @export
results.gd_job_prog = function(gd, ...) {
  httr::warn_for_status(gd$response)
  if (!gd$content$success) {
    warning("Glassdoor did not indicate successful retrieval!")
  }
  response = gd$content$response
  n_res = names(response)
  response = response[ !n_res %in% "results"]
  l_res = vapply(response, length, FUN.VALUE = integer(1))
  length_one = l_res == 1
  if (!all(length_one)) {
    msg = paste0(
      "Field(s) ",
      paste(n_res[!length_one], collapse = ", "),
      " will be removed, not of length 1")
    warning(msg)
    response = response[ !length_one]
  }
  response = unlist(response)
  numerics = c("payLow", "payMedian", "payHigh")

  make_df = function(x, numerics) {
    x = t(x)
    x = as.data.frame(x, stringsAsFactors = FALSE)

    for (inum in numerics) {
      x[, inum] = as.numeric(x[, inum, drop = TRUE])
    }
    return(x)
  }
  response = make_df(response, numerics)

  res = vapply(
    gd$content$response$results, unlist,
    FUN.VALUE = character(5)
  )
  numerics = c("frequency", "frequencyPercent", "nationalJobCount",
               "medianSalary")
  res = make_df(res, numerics)

  L = list(
    progression_table = res,
    job_info = response
  )
  return(L)
}