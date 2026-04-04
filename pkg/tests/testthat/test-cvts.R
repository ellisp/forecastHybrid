# Unit tests on the cvts function
naiveForecast <- function(train) {
  result <- list()
  result$series <- train
  result$forecast <- train[length(train)]
  class(result) <- "naive_model"
  result
}

forecastFunction <- function(model, h = 12) {
  result <- list()
  result$model <- model
  freq <- tsp(model$series)[3]
  result$mean <- rep(model$forecast, h)
  tsp(result$mean) <- c(tsp(model$series)[2] + 1 / freq, tsp(model$series)[2] + h / freq,
                        freq)
  class(result) <- "forecast"
  result
}

context("Testing input for cvts()")
test_that("Testing invalid inputs", {
  expect_error(cvts("invalid"))
  # useHorizon must be > 0
  expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, useHorizon = 0L))
  # windowSize should be an integer
  expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, windowSize = 3.2))
  expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, windowSize = 130,
                    maxHorizon = 12))
  # windowSize must be > 0
  expect_error(cvts(AirPassengers, windowSize = 0))
  # maxHorizon must be > 0
  expect_error(cvts(AirPassengers, maxHorizon = 0))
})

test_that("Testing valid inputs", {
  expect_error(cvts(USAccDeaths, FUN = thetam, FCFUN = forecast, rolling = FALSE,
                    windowSize = 48, maxHorizon = 12), NA)
  expect_error(cvts(USAccDeaths, FUN = thetam, FCFUN = forecast, rolling = TRUE,
                    windowSize = 48, maxHorizon = 12, verbose = FALSE), NA)
  expect_error(cvts(rnorm(94), saveModels = FALSE, saveForecasts = FALSE), NA)
})

test_that("Testing accuracy.cvts()", {
  inputSeries <- ts(rnorm(8), f = 2)
  cv <- cvts(inputSeries, windowSize = 4, maxHorizon = 2)
  expect_error(accuracy(cv), NA)
})

# These tests are more compute intensive and cran push over the CRAN allowed time limit.
skip_on_cran()

test_that("Rolling forecasts work", {
  cv <- cvts(AirPassengers, FUN = naiveForecast, FCFUN = forecastFunction,
             rolling = TRUE, windowSize = 1, maxHorizon = 1)

  forecasts <- vapply(cv$forecasts, function(x) x[[2]], numeric(1))
  trainSeries <- vapply(cv$forecasts, function(x) x[[1]]$series, numeric(1))
  expect_identical(AirPassengers[1:(length(AirPassengers) - 1)], trainSeries)
  expect_identical(AirPassengers[1:(length(AirPassengers) - 1)], forecasts)
})

test_that("Additional parameters can be passed to fitting functions", {
  cv <- cvts(AirPassengers, FUN = ets, FCFUN = forecast, rolling = FALSE, windowSize = 12,
             maxHorizon = 12, model = "MAM")

  etsFit <- ets(window(AirPassengers, end = c(1959, 12)), model = "MAM")

  ## The call objects alone are different seemingly because of the do.call used in cvts
  # so tests for identical result objects won't work
  etsWithCall <- forecast(etsFit, 12)
  etsWithoutCall <- etsWithCall[setdiff(names(etsWithCall), c("model", "call"))]
  fcLastWithoutCall <- cv$forecasts[[11]][setdiff(names(cv$forecasts[[11]]),
                                                  c("model", "call"))]
  # Fitted values and confidence intervals should be the same, however
  expect_true(all.equal(fcLastWithoutCall$lower, etsWithoutCall$lower))
  expect_true(all.equal(fcLastWithoutCall$mean, etsWithoutCall$mean))
  expect_true(all.equal(fcLastWithoutCall$upper, etsWithoutCall$upper))
  expect_true(all.equal(fcLastWithoutCall$method, etsWithoutCall$method))
  expect_true(all.equal(fitted(fcLastWithoutCall), fitted(etsWithoutCall)))
})

test_that("Extract forecasts works", {
  cv <- cvts(AirPassengers, FUN = naiveForecast, FCFUN = forecastFunction, rolling = TRUE,
             windowSize = 1, maxHorizon = 1)

  laggedForecasts <- window(lag(extractForecasts(cv, 1)), start = c(1949L, 1L))
  orig <- window(AirPassengers, end = c(1960L, 11L))

  expect_equal(laggedForecasts, orig)
})

test_that("Time series partitions work", {
  slices <- tsPartition(AirPassengers, TRUE, 1, 1)
  trainIndices <- Map(function(x) x$trainIndices, slices)
  allTrainIndices <- Reduce(c, trainIndices)
  expect_identical(allTrainIndices, seq(1L, length(AirPassengers) - 1, 1L))

  testIndices <- Map(function(x) x$testIndices, slices)
  allTestIndices <- Reduce(c, testIndices)
  expect_identical(allTestIndices, seq(2L, length(AirPassengers), 1L))
})

test_that("xreg is properly used", {
  ## Ensure xreg is ignored when a model that does not accept xreg is used
  series <- ts(rnorm(14), f = 2)
  maxHorizon <- 2
  windowSize <- 7
  expect_warning(cvts(series, FUN = thetam, windowSize = windowSize, maxHorizon = maxHorizon,
                      xreg = data.frame(x = rnorm(series))),
                 "Ignoring xreg parameter since fitting function does not accept xreg")

  ## Ensure xreg is ignored when NULL
  cv <- cvts(series, nnetar, xreg = NULL, windowSize = windowSize, maxHorizon = maxHorizon)
  xregForEachModel <- Map(function(x) x$xreg, cv$models)
  xregAllModels <- Reduce(c, xregForEachModel)
  expect_null(xregAllModels)

  ## Ensure xreg is used when a model accepts xreg and xreg is a vector
  series <- ts(rnorm(144), f = 2)
  maxHorizon <- 2
  windowSize <- 136
  xreg <-  runif(length(series))
  cv <- cvts(series, nnetar, maxHorizon = maxHorizon,
             xreg = xreg, windowSize = windowSize)

  xregsAllModels <- Map(function(x) x$xreg, cv$models)
  xregsLast <- xregsAllModels[[length(xregsAllModels)]]
  expect_identical(xreg[1:(length(series) - maxHorizon)], as.numeric(xregsLast))
})

test_that("custom FUN and FCFUN", {
  # stlm from "forecast" package
  FUN <- function(x) forecast::stlm(x) # nolint
  FCFUN <- function(x, h) forecast::forecast(x, h = h) # nolint
  series <- wineind
  windowSize <- 152
  expect_error(cvts(wineind, FUN = FUN, FCFUN = FCFUN, windowSize = windowSize), NA)
  # lm from "stats" package
  FCFUN <- function(x, h) { # nolint
    dat <- data.frame(x = seq_along(x), y = x)
    mod <- lm(y ~ x, data = dat)
    newx <- data.frame(x = (length(x) + 1):(length(x) + h))
    pred <- predict(mod, newx)
    result <- list()
    result$mean <- pred
    result
  }
  series <- ts(rnorm(6), f = 2)
  expect_error(cvts(series, FCFUN = FCFUN, windowSize = 4, maxHorizon = 1), NA)
})

test_that("examples from docs", {
  cvmod2 <- cvts(USAccDeaths, FUN = ets,
                 saveModels = FALSE, saveForecasts = FALSE,
                 windowSize = 36, maxHorizon = 12)
  expect_length(cvmod2, 6L)

  cvmod3 <- cvts(AirPassengers, FUN = hybridModel,
                 FCFUN = function(mod, h) forecast(mod, h = h, PI = FALSE),
                 rolling = FALSE, windowSize = 48,
                 maxHorizon = 12)
  expect_length(cvmod3, 6L)
})

test_that("parity when 1 vs 2 cores used", {
  series <- ts(rnorm(10), f = 2)
  cvSerial <- cvts(series, FUN = stlm, windowSize = 6, maxHorizon = 2, num.cores = 1)
  cvParallel <- cvts(series, FUN = stlm, windowSize = 6, maxHorizon = 2, num.cores = 2)
  expect_named(cvSerial, names(cvParallel))
  expect_null(cvSerial$xreg)
  expect_null(cvParallel$xreg)
  expect_identical(cvSerial$x, cvParallel$x)
  expect_identical(cvSerial$residuals, cvParallel$residuals)
  expect_identical(cvSerial$forecasts, cvParallel$forecasts)
  expect_true(all.equal(length(cvSerial$models), length(cvParallel$models), 3))
})
