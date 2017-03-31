# Unit tests on the cvts function
if(require(forecast) &  require(testthat)){
   naive_forecast <- function(train) {
      result <- list()
      result$series <- train
      result$forecast <- train[length(train)]
      class(result) <- "naive_model"
      return(result)
   }
   
   forecastFunction <- function(model, h = 12) {
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
    expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, useHorizon = 0L))
    expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, windowSize = 3.2))
    expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, windowSize = 130, maxHorizon = 12))
  })
  test_that("Testing valid inputs", {
     expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, rolling = FALSE, windowSize = 60, maxHorizon = 12), NA)
     expect_error(cvts(AirPassengers, FUN = thetam, FCFUN = forecast, rolling = TRUE, windowSize = 60, maxHorizon = 12, verbose = FALSE), NA)
     expect_error(cvts(rnorm(100), saveModels = FALSE, saveForecasts = FALSE), NA)
  })
  test_that("Testing accuracy.cvts()", {
    inputSeries <- ts(rnorm(30), f = 4)
    cv <- cvts(inputSeries, windowSize = 8, maxHorizon = 8)
    expect_error(accuracy(cv), NA)
  })
  test_that("Rolling forecasts work", {
     cv <- cvts(AirPassengers, FUN = naive_forecast, FCFUN = forecastFunction, rolling = TRUE, windowSize = 1,
                maxHorizon = 1)
     
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
      expect_true(all(fc_last_without_call$mean == ets_without_call$mean))
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
       expect_warning(cvts(AirPassengers, ets, xreg = data.frame(x = rnorm(AirPassengers))), "Ignoring xreg parameter since fitting function does not accept xreg")

       ## Ensure xreg is ignored when NULL
       cv <- cvts(AirPassengers, nnetar, xreg = NULL, windowSize = 100)
       xregForEachModel <- Map(function (x) x$xreg, cv$models)
       xregAllModels <- Reduce(c, xregForEachModel)
       expect_identical(xregAllModels, NULL)

       ## Ensure xreg is used when a model accepts xreg and xreg is a vector
       xreg <-  rnorm(length(AirPassengers))
       maxHorizon <- 2
       cv <- cvts(AirPassengers, nnetar, maxHorizon = maxHorizon,
                  xreg = xreg, windowSize = 100)

       xregsAllModels <- Map(function(x) x$xreg, cv$models)
       xregsLast <- xregsAllModels[[length(xregsAllModels)]]
       expect_equal(xreg[1:(length(AirPassengers) - maxHorizon)], as.numeric(xregsLast))
   })
 }
