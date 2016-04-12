# Unit tests on the hybridModel function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    hModel <- hybridModel(woolyrnq)
    expect_error(forecast.hybridModel(object = 1))
    expect_error(forecast.hybridModel(object = hModel, h = -1L))
    expect_error(forecast.hybridModel(object = hModel, h = "a"))
    expect_error(forecast.hybridModel(object = hModel, h = 3.2))
    expect_error(forecast.hybridModel(object = hModel, h = 5, xreg = matrix(1:5, nrow = 5, ncol = 2)))
  })
  test_that("Testing forecasts with xreg", {
    mm <- matrix(runif(length(wineind)), nrow = length(wineind))
    expect_error(aa <- hybridModel(wineind, models = "as", a.args = list(xreg = mm), s.args = list(xreg = mm)))
    aa <- hybridModel(wineind, models = "ans", a.args = list(xreg = mm), n.args = list (xreg = mm))
    expect_warning(tmp <- forecast(aa, xreg = matrix(rnorm(20), nrow = 20)))
    expect_warning(forecast(aa, h = 10, xreg = matrix(rnorm(20), nrow = 20)))
    expect_that(forecast(aa, xreg = matrix(rnorm(24), nrow = 24)), not(throws_error()))
    expect_true(length(forecast(aa, xreg = matrix(rnorm(24), nrow = 24))$mean) == 24L)
    expect_true(class(forecast(aa, xreg = matrix(rnorm(24), nrow = 24))) == "forecast")
  })
}
