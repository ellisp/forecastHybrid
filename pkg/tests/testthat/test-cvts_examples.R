# Unit tests on the cvts function
if(require(forecast) & require(testthat)){
  context("Testing cvts() examples")

  test_that("Testing skipped examples", {
    series <- subset(AirPassengers, end = 26)
    rwcv <- cvts(series, FCFUN = rwf, windowSize = 24, maxHorizon = 1)
    expect_equal(length(rwcv$forecasts), length(rwcv$models))
    expect_equal(length(rwcv$forecasts), 2)
    expect_equal(dim(residuals(rwcv)), c(1, 1))

    cvmod2 <- cvts(USAccDeaths, FUN = stlm,
                   saveModels = FALSE, saveForecasts = FALSE,
                   windowSize = 36, maxHorizon = 12)
    expect_null(cvmod2$forecasts)
    expect_null(cvmod2$models)
    expect_equal(dim(residuals(cvmod2)), c(3, 12))

    series <- subset(AirPassengers, end=40)
    cvmod3 <- cvts(series, FUN = hybridModel,
                   FCFUN = function(mod, h) forecast(mod, h = h, PI = FALSE),
                   rolling = FALSE, windowSize = 36,
                   maxHorizon = 2)
    expect_equal(length(cvmod3$forecasts), length(cvmod3$models))
    expect_equal(length(cvmod3$forecasts), 2)
    expect_equal(dim(residuals(cvmod3)), c(2, 2))
    
  })
 }
