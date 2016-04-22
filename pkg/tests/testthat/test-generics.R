# Unit tests on the generic function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing generic functions")
  set.seed(2345)
  expect_warning(hm <- hybridModel(y = 10 + rnorm(100), models = "aenst"))
  test_that("Testing summary and print methods", {
    expect_that(summary(hm), not(throws_error()))
    expect_that(print(hm), not(throws_error()))
    expect_true(is.hybridModel(hm))
  })
  test_that("Testing fitted and residual methods", {
    expect_that(fitted(hm), not(throws_error()))
    expect_that(residuals(hm), not(throws_error()))
    expect_that(fitted(hm, individual = TRUE), not(throws_error()))
    expect_that(residuals(hm, individual = TRUE), not(throws_error()))
    expect_true(length(fitted(hm)) == 100L)
    expect_true(length(residuals(hm)) == 100L)
    expect_true(length(residuals(hm, individual = TRUE)$nnetar) == 100L)
    expect_true(length(fitted(hm, individual = TRUE)$tbats) == 100L)
  })
}
