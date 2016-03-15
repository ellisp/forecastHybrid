# Unit tests on the generic function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing generic functions")
  set.seed(2345)
  hm <- hybridModel(y = 10 + rnorm(100), models = "aten")
  test_that("Testing summary and print methods", {
    expect_that(summary(hm), not(throws_error()))
    expect_that(print(hm), not(throws_error()))
    expect_true(is.hybridModel(hm))
  })
  test_that("Testing fitted and residual methods", {
    expect_that(fitted(hm), not(throws_error()))
    expect_that(residuals(hm), not(throws_error()))
    expect_true(length(fitted(hm)) == 4L)
    expect_true(length(residuals(hm)) == 4L)
    expect_true(length(residuals(hm)$nnetar) == 100L)
    expect_true(length(fitted(hm)$tbats) == 100L)
  })
}