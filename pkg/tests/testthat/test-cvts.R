# Unit tests on the cvts function
if(require(forecast) & require(testthat)){
   naive_forecast <- function(train){
      result <- list()
      result$series <- train
      result$forecast <- train[length(train)]
      class(result) <- "naive_model"
      return(result)
   }

   forecastFunction <- function(model, h = 12){
      result <- list()
      result$model <- model
      freq <- tsp(model$series)[3]
      result$mean <- rep(model$forecast, h)
      tsp(result$mean) <- c(tsp(model$series)[2] + 1/freq, tsp(model$series)[2] + h/freq,
                            freq)
      class(result) <- "forecast"
      return(result)
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
  test_that("Rolling forecasts work", {
     cv <- cvts(AirPassengers, FUN = naive_forecast, FCFUN = forecastFunction,
                rolling = TRUE, windowSize = 1, maxHorizon = 1)

     forecasts <- vapply(cv$forecasts, function(x) x[[2]], numeric(1))
     train_series <- vapply(cv$forecasts, function(x) x[[1]]$series, numeric(1))
     expect_equal(AirPassengers[1:(length(AirPassengers) - 1)], train_series)
     expect_equal(AirPassengers[1:(length(AirPassengers) - 1)], forecasts)
   })

   test_that("Additional parameters can be passed to fitting functions", {
      cv <- cvts(AirPassengers, FUN = ets, FCFUN = forecast, rolling = FALSE, windowSize = 12,
                 maxHorizon = 12, model = "MAM")

      fc_last <- cv$forecasts[[11]]
      ets_fit <- ets(window(AirPassengers, end = c(1959, 12)), model = "MAM")

      ## The call objects alone are different seemingly because of the do.call used in cvts
      ets_with_call <- forecast(ets_fit, 12) 
      ets_without_call <- ets_with_call[setdiff(names(ets_with_call), c("model", "call"))]
      fc_last_without_call <- cv$forecasts[[11]][setdiff(names(cv$forecasts[[11]]), 
                                                         c("model", "call"))]

      #expect_identical(ets_without_call, fc_last_without_call)
      # use a more relaxed test for now
      expect_true(all.equal(fc_last_without_call$mean, ets_without_call$mean))
   })

   test_that("Extract forecasts works", {

      cv <- cvts(AirPassengers, FUN = naive_forecast, FCFUN = forecastFunction, rolling = TRUE, 
                 windowSize = 1, maxHorizon = 1)

      lagged_forecasts <- window(lag(extractForecasts(cv, 1)), start = c(1949, 1))
      orig <- window(AirPassengers, end = c(1960, 11))

      expect_equal(lagged_forecasts, orig)
   })

   test_that("Time series partitions work", {

       slices <- tsPartition(AirPassengers, TRUE, 1, 1)
       trainIndices <- Map(function(x) x$trainIndices, slices)
       allTrainIndices <- Reduce(c, trainIndices)
       expect_equal(allTrainIndices, seq(1, length(AirPassengers) - 1, 1))

       testIndices <- Map(function(x) x$testIndices, slices)
       allTestIndices <- Reduce(c, testIndices)
       expect_equal(allTestIndices, seq(2, length(AirPassengers), 1))
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
       xregForEachModel <- Map(function (x) x$xreg, cv$models)
       xregAllModels <- Reduce(c, xregForEachModel)
       expect_identical(xregAllModels, NULL)

       ## Ensure xreg is used when a model accepts xreg and xreg is a vector
       series <- ts(rnorm(144), f = 2)
       maxHorizon <- 2
       windowSize <- 136
       xreg <-  runif(length(series))
       cv <- cvts(series, nnetar, maxHorizon = maxHorizon,
                  xreg = xreg, windowSize = windowSize)

       xregsAllModels <- Map(function(x) x$xreg, cv$models)
       xregsLast <- xregsAllModels[[length(xregsAllModels)]]
       expect_equal(xreg[1:(length(series) - maxHorizon)], as.numeric(xregsLast))
   })
   test_that("custom FUN and FCFUN", {
     # stlm from "forecast" package
     FUN <- function(x){forecast::stlm(x)}
     FCFUN <- function(x, h){forecast::forecast(x, h = h)}
     series <- wineind
     windowSize = 152
     expect_error(cvts(wineind, FUN = FUN, FCFUN = FCFUN, windowSize = windowSize), NA)
     # lm from "stats" package
     FCFUN <- function(x, h){
       dat <- data.frame(x = 1:length(x), y = x)
       mod <- lm(y ~ x, data = dat)
       newx <- data.frame(x = (length(x) + 1):(length(x) + h))
       pred <- predict(mod, newx)
       result <- list()
       result$mean <- pred
       return(result)
       }
     series <- ts(rnorm(6), f = 2)
     expect_error(cvts(series, FCFUN = FCFUN, windowSize = 4, maxHorizon = 1), NA)
   })
 }
