# Unit tests on the hybridModel function
if(require(forecast) &  require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    inputSeries <- ts(rnorm(15), f = 7)
    hModel <- hybridModel(inputSeries)
    expect_error(hybridModel(1:3, models = "ae"))
    # h must be positive
    expect_error(forecast(object = hModel, h = -1L))
    # h must be numeric
    expect_error(forecast(object = hModel, h = "a"))
    # h should be an integer
    expect_error(forecast(object = hModel, h = 3.2))
    # matrix should be numeric
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = matrix("a", nrow = 5, ncol = 2)))
    expect_error(forecast(object = hModel, h = 5,
                                     xreg = 1:12))
    # s3 forecast method should take a hybridModel object only
    expect_error(forecast.hybridModel("a"))
  })

  test_that("Testing forecasts with xreg", {
    # Test a simple et model
    inputSeries <- ts(rnorm(9), f = 4)
    hm <- hybridModel(inputSeries, models = "et")
    expect_error(forecast(hm), NA)

    # Test xregs
    inputSeries <- ts(wineind[1:25], f = frequency(wineind))
    mm <- matrix(runif(length(inputSeries)), nrow = length(inputSeries))
    # stlm only works with xreg when method = "arima" is passed in s.args
    expect_error(aa <- hybridModel(inputSeries, models = "afns",
                                   a.args = list(xreg = mm),
                                   s.args = list(xreg = mm)))
    aa <- hybridModel(inputSeries, models = "aefnst",
                      a.args = list(xreg = mm),
                      n.args = list (xreg = mm),
                      s.args = list(xreg = mm, method = "arima"))
    # If xreg is used and no h is provided, overwrite h
    newXreg <- matrix(rnorm(20), nrow = 20)
    expect_error(tmp <- forecast(aa, xreg = newXreg, npaths = 5), NA)
    # If nrow(xreg) != h, issue a warning but set h <- nrow(xreg)
    expect_warning(forecast(aa, h = 10, xreg = newXreg, PI = FALSE))
    newXreg <- matrix(rnorm(24), nrow = 24)
    
    # Fit the model using xreg for only one individual component
    # Forecast should still work (previous bug)
    mod <- hybridModel(inputSeries, models = "af", a.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg), NA)
    mod <- hybridModel(inputSeries, models = "nf", n.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg, PI = FALSE), NA)
    mod <- hybridModel(inputSeries, models = "sf", s.args = list(xreg = mm, method = "arima"))
    expect_error(forecast(mod, xreg = newXreg), NA)

    # Valid forecast properties
    expect_error(forecast(aa, xreg = matrix(rnorm(24), nrow = 24), PI = FALSE), NA)
    expect_true(length(forecast(aa, xreg = newXreg, PI = FALSE)$mean) == 24L)
    expect_true(class(forecast(aa, xreg = newXreg, PI = FALSE)) == "forecast")
    # Prediction intervals for nnetar are nondeterministic, so this will fail
    # Testing this is slow, so leave it out for now
    #expect_true(all(forecast(aa, xreg = mm,  h = nrow(mm), level = 0.9)$upper == forecast(aa, xreg = mm,  h = nrow(mm), level = 90)$upper))
    expect_error(forecast(aa, xreg = mm, level = 110))
    # Fan should generate 17 prediction intervals
    expect_true(ncol(forecast(aa, xreg = mm, h = nrow(mm), fan = TRUE)$upper) == 17)
  })
}
