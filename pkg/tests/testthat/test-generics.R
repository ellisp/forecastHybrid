# Unit tests on the generic function
if(require(forecast) & require(testthat)){
  context("Testing generic functions")
  set.seed(2345)
  expect_warning(hm <- hybridModel(y = 10 + rnorm(100), models = "aenst"))
  test_that("Testing summary and print methods", {
    expect_error(summary(hm), NA)
    expect_error(print(hm), NA)
    expect_true(is.hybridModel(hm))
  })
  test_that("Testing fitted and residual methods", {
    expect_error(fitted(hm), NA)
    expect_error(residuals(hm), NA)
    expect_error(fitted(hm, individual = TRUE), NA)
    expect_error(residuals(hm, individual = TRUE), NA)
    expect_true(length(fitted(hm)) == 100L)
    expect_true(length(residuals(hm)) == 100L)
    expect_true(length(residuals(hm, individual = TRUE)$nnetar) == 100L)
    expect_true(length(fitted(hm, individual = TRUE)$tbats) == 100L)
  })
}
