test_that("Glassdoor PAT", {
  if (!have_glassdoor_pat()) {
    expect_error(glassdoor_pat(error = TRUE))
  } else {
    expect_silent(glassdoor_pat(error = TRUE))
  }
  expect_error(glassdoor_pat(token = "", error = TRUE))

})


test_that("Glassdoor PID", {
  if (!have_glassdoor_pid()) {
    expect_error(glassdoor_pid(error = TRUE))
  } else {
    expect_silent(glassdoor_pid(error = TRUE))
  }
  expect_error(glassdoor_pid(token = "", error = TRUE))

})
#
# res = glassdoor_api(
#   action = "employers",
#   other = NULL,
#   version = 1,
#   format = "json",
#   query = list(q = "pharmaceuticals"))
# }