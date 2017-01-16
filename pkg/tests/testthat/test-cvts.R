# Unit tests on the cvts function
if(require(forecast) &  require(testthat)){
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
     naive_forecast <- function(train) {
        result <- list()
        result$series <- train
        result$forecast <- train[length(train)]
        class(result) <- "naive_model"
        return(result)
     }
     
     forecast.naive_model <- function(model, h = 12) {
        result <- list()
        result$model <- model
        result$mean <- rep(model$forecast, h)
        class(result) <- "forecast"
        return(result)
     }
     
     cv <- cvts(AirPassengers, FUN = naive_forecast, FCFUN = forecast.naive_model, rolling = TRUE, windowSize = 1,
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
      ets_without_call <- forecast(ets_fit, 12) %>% {.[setdiff(names(.), c("model", "call"))]}
      fc_last_without_call <- cv$forecasts[[11]][setdiff(names(cv$forecasts[[11]]), c("model", "call"))]
      
      expect_identical(ets_without_call, fc_last_without_call)
   })
   
   test_that("Extract rolling forecasts works", {
      naive_forecast <- function(train) {
         result <- list()
         result$series <- train
         result$forecast <- train[length(train)]
         class(result) <- "naive_model"
         return(result)
      }
      
      forecast.naive_model <- function(model, h = 12) {
         result <- list()
         result$model <- model
         freq <- tsp(model$series)[3]
         result$mean <- rep(model$forecast, h)
         tsp(result$mean) <- c(tsp(model$series)[2] + 1/freq, tsp(model$series)[2] + h/freq,
                                freq) 
                           
         class(result) <- "forecast"
         return(result)
      }
      
      cv <- cvts(AirPassengers, FUN = naive_forecast, FCFUN = forecast, rolling = TRUE, windowSize = 1,
                 maxHorizon = 1)
      
      forecasts <- extract_rolling_forecasts(cv) %>%
         lag() 
      orig <- window(AirPassengers, end = c(1960, 11))
      
      expect_equal(forecasts, orig)
   })
}
