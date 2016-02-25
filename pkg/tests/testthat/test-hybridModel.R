# Unit tests on the hybridModel function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing input for hybridModel()")
  test_that("Testing invalid inputs", {
    expect_that(hybridModel(y = 1:10, models = "jten"), throws_error())
    expect_that(hybridModel(y = 1:10, models = 5), throws_error())
    expect_that(hybridModel(y = matrix(1:10, nrow = 5, ncol = 2), models = 5), throws_error())
    expect_that(hybridModel(wineind, xreg = rnorm(length(wineind) - 1)), throws_error())
    expect_that(hybridModel(y = "hello world"), throws_error())
    expect_that(hybridModel(), throws_error())
  })
}
