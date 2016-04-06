# Unit tests on the hybridModel function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing input for hybridModel()")
  test_that("Testing invalid inputs", {
    expect_error(hybridModel(y = 1:10, models = "jten"))
    expect_error(hybridModel(y = 1:10, models = 5))
    expect_error(hybridModel(y = matrix(1:10, nrow = 5, ncol = 2), models = 5))
    # Test for invalid mistmatch length of y and xreg in a.args/s.args later?
    expect_error(hybridModel(y = wineind, a.args = list(xreg = rnorm(length(wineind) - 1))))
    expect_error(hybridModel(y = "hello world"))
    expect_error(hybridModel())
    expect_error(hybridModel(y = numeric()))
    expect_error(hybridModel(y = "aten"))
    expect_error(hybridModel(wineind, models = "a"))
    expect_error(hybridModel(wineind, num.cores = -1L))
    expect_error(hybridModel(wineind, num.cores = 3.3))
    expect_error(hybridModel(wineind, num.cores = "a"))
    expect_error(hybridModel(wineind, models = ""))
    
  })
  test_that("Testing valid inputs", {
    set.seed(54321)
    expect_that(hybridModel(y = rnorm(100), models = "ae", a.args = list(xreg = matrix(runif(100), nrow = 100))), not(throws_error()))
    expect_warning(hybridModel(y = rnorm(10), models = "en", a.args = list(lambda = 0.5)))
    expect_that(hybridModel(wineind, models = "atens"), not(throws_error()))
  })
  test_that("Testing model matching", {
    set.seed(123456)
    expect_that(hybridModel(y = rnorm(20), models = "AAAAE"), not(throws_error()))
    expect_that(hybridModel(y = rnorm(20), models = "AtTaaa"), not(throws_error()))
    expect_that(hybridModel(y = rnorm(20), models = "nNeTEEEeA"), not(throws_error()))
  })
  context("Testing generic functions")
  test_that("Testing is.hybridModel(), fitted.hybridModel(), and residuals.hybridModel()", {
    exampleModel <- hybridModel(wineind)
    expect_true(is.hybridModel(exampleModel))
    expect_true(length(fitted(exampleModel)) == length(residuals(exampleModel)))
    expect_true(length(fitted(exampleModel, individual = TRUE)) == length(residuals(exampleModel, individual = TRUE)))
  })
}
