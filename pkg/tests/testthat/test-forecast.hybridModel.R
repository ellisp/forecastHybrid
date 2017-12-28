# Unit tests on the hybridModel function
if(require(forecast) &  require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    inputSeries <- ts(rnorm(6), f = 2)
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
    inputSeries <- ts(rnorm(5), f = 2)
    hm <- hybridModel(inputSeries, models = "et")
    expect_error(forecast(hm), NA)

    # Test xregs
    inputSeries <- subset(wineind, end = 25)
    mm <- matrix(runif(length(inputSeries)), nrow = length(inputSeries))
    # stlm only works with xreg when method = "arima" is passed in s.args
    expect_error(aa <- hybridModel(inputSeries, models = "afns",
                                   a.args = list(xreg = mm),
                                   s.args = list(xreg = mm)))
    aa <- hybridModel(inputSeries, models = "aefnst",
                      a.args = list(xreg = mm),
                      n.args = list(xreg = mm),
                      s.args = list(xreg = mm, method = "arima"))
    # If xreg is used and no h is provided, overwrite h
    newXreg <- matrix(rnorm(3), nrow = 3)
    expect_error(tmp <- forecast(aa, xreg = newXreg, npaths = 5), NA)
    expect_equal(class(tmp), "forecast")
    expect_equal(length(tmp$mean), nrow(newXreg))
    # If nrow(xreg) != h, issue a warning but set h <- nrow(xreg)
    expect_warning(forecast(aa, h = 10, xreg = newXreg, PI = FALSE))
    
    # Fit the model using xreg for only one individual component
    # Forecast should still work (previous bug)
    mod <- hybridModel(inputSeries, models = "af", a.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg), NA)
    mod <- hybridModel(inputSeries, models = "nf", n.args = list(xreg = mm))
    expect_error(forecast(mod, xreg = newXreg, PI = FALSE), NA)
    mod <- hybridModel(inputSeries, models = "sf", s.args = list(xreg = mm, method = "arima"))
    expect_error(forecast(mod, xreg = newXreg), NA)

    # Valid forecast properties
    expect_error(forecast(aa, xreg = newXreg, PI = FALSE), NA)
    expect_true(length(forecast(aa, xreg = newXreg, PI = FALSE)$mean) == nrow(newXreg))
    expect_true(class(forecast(aa, xreg = newXreg, PI = FALSE)) == "forecast")
    # Prediction intervals for nnetar are nondeterministic, so this will fail
    # Testing this is slow, so leave it out for now
    #expect_true(all(forecast(aa, xreg = mm,  h = nrow(mm), level = 0.9)$upper == forecast(aa, xreg = mm,  h = nrow(mm), level = 90)$upper))
    expect_error(forecast(aa, xreg = mm, level = 110))
    # Fan should generate 17 prediction intervals
    fc <- forecast(aa, xreg = newXreg, h = nrow(newXreg), fan = TRUE)
    expect_true(ncol(fc$upper) == 17)
    expect_true(ncol(fc$lower) == 17)
  })
}
