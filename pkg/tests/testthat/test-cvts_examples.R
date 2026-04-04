# Unit tests on the cvts function
context("Testing cvts() examples")

test_that("Testing skipped examples", {
  series <- subset(AirPassengers, end = 26)
  rwcv <- cvts(series, FCFUN = rwf, windowSize = 24, maxHorizon = 1)
  expect_length(rwcv$forecasts, length(rwcv$models))
  expect_length(rwcv$forecasts, 2L)
  expect_identical(dim(residuals(rwcv)), c(1L, 1L))

  cvmod2 <- cvts(USAccDeaths, FUN = stlm,
                 saveModels = FALSE, saveForecasts = FALSE,
                 windowSize = 36, maxHorizon = 12)
  expect_null(cvmod2$forecasts)
  expect_null(cvmod2$models)
  expect_identical(dim(residuals(cvmod2)), c(3L, 12L))

  series <- subset(AirPassengers, end = 40)
  cvmod3 <- cvts(series, FUN = hybridModel,
                 FCFUN = function(mod, h) forecast(mod, h = h, PI = FALSE),
                 rolling = FALSE, windowSize = 36,
                 maxHorizon = 2)
  expect_length(cvmod3$forecasts, length(cvmod3$models))
  expect_length(cvmod3$forecasts, 2)
  expect_identical(dim(residuals(cvmod3)), c(2L, 2L))
})
