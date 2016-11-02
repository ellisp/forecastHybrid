# Unit tests on the hybridModel function
if(require(forecast) &  require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    hModel <- hybridModel(woolyrnq)
    expect_error(hybridModel(1:3, models = "ae"))
    expect_error(forecast(object = hModel, h = -1L))
    expect_error(forecast(object = hModel, h = "a"))
    expect_error(forecast(object = hModel, h = 3.2))
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = matrix(1:5, nrow = 5, ncol = 2)))
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = matrix("a", nrow = 5, ncol = 2)))
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = 1:12))
  })
  test_that("Testing forecasts with xreg", {
    mm <- matrix(runif(length(wineind)), nrow = length(wineind))
    expect_error(aa <- hybridModel(wineind, models = "aenst", a.args = list(xreg = mm), s.args = list(xreg = mm)))
    aa <- hybridModel(wineind, models = "aenst", a.args = list(xreg = mm), n.args = list (xreg = mm))
    # If xreg is used and no h is provided, overwrite h
    expect_error(tmp <- forecast(aa, xreg = matrix(rnorm(20), nrow = 20)), NA)
    # If nrow(xreg) != h, issue a warning but set h <- nrow(xreg)
    expect_warning(forecast(aa, h = 10, xreg = matrix(rnorm(20), nrow = 20), PI = FALSE))
    expect_error(forecast(aa, xreg = matrix(rnorm(24), nrow = 24), PI = FALSE), NA)
    expect_true(length(forecast(aa, xreg = matrix(rnorm(24), nrow = 24), PI = FALSE)$mean) == 24L)
    expect_true(class(forecast(aa, xreg = matrix(rnorm(24), nrow = 24), PI = FALSE)) == "forecast")
    # Prediction intervals for nnetar are nondeterministic, so this will fail
    #expect_true(all(forecast(aa, xreg = mm,  h = nrow(mm), level = 0.9)$upper == forecast(aa, xreg = mm,  h = nrow(mm), level = 90)$upper))
    expect_error(forecast(aa, xreg = mm, level = 110))
    # Prediction intervals for nnetar are nondeterministic, so this will fail
    #expect_true(ncol(forecast(aa, xreg = mm, h = nrow(mm), fan = TRUE)$upper) == 17)
  })
}
