test_that("Glassdoor PAT", {
  if (!have_gd_pat()) {
    expect_error(gd_pat(error = TRUE))
  } else {
    expect_silent(gd_pat(error = TRUE))
  }
  expect_error(gd_pat(token = "", error = TRUE))

})


test_that("Glassdoor PID", {
  if (!have_gd_pid()) {
    expect_error(gd_pid(error = TRUE))
  } else {
    expect_silent(gd_pid(error = TRUE))
  }
  expect_error(gd_pid(token = "", error = TRUE))

})
#
# res = gd_api(
#   action = "employers",
#   other = NULL,
#   version = 1,
#   format = "json",
#   q = "pharmaceuticals")
# }