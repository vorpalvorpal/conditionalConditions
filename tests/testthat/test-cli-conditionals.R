testthat::test_that("abort_if and abort_if_not work correctly", {
  testthat::expect_error(abort_if(TRUE, "Error message"), "Error message")
  testthat::expect_no_error(abort_if(FALSE, "Error message"))
  testthat::expect_error(abort_if_not(FALSE, "Error message"), "Error message")
  testthat::expect_no_error(abort_if_not(TRUE, "Error message"))
})

testthat::test_that("warn_if and warn_if_not work correctly", {
  testthat::expect_warning(warn_if(TRUE, "Warning message"), "Warning message")
  testthat::expect_no_warning(warn_if(FALSE, "Warning message"))
  testthat::expect_warning(warn_if_not(FALSE, "Warning message"), "Warning message")
  testthat::expect_no_warning(warn_if_not(TRUE, "Warning message"))
})

testthat::test_that("inform_if and inform_if_not work correctly", {
  testthat::expect_message(inform_if(TRUE, "Info message"), "Info message")
  testthat::expect_no_message(inform_if(FALSE, "Info message"))
  testthat::expect_message(inform_if_not(FALSE, "Info message"), "Info message")
  testthat::expect_no_message(inform_if_not(TRUE, "Info message"))
})
