#' Cross validation for time series
#'
#' Perform cross validation on a time series.
#'
#' @import doParallel
#' @import foreach
#' @export
#' @param x the input time series.
#' @param FUN the model function used. Custom functions are allowed. See details and examples.
#' @param FCFUN a function that process point forecasts for the model function. This defaults to \code{\link{forecast}}. Custom functions are allowed. See details and examples.
#' See details.
#' @param rolling should a rolling procedure be used? If TRUE, nonoverlapping windows of size \code{maxHorizon}
#' will be used for fitting each model. If FALSE, the size of the dataset used for training will grow
#' by one each iteration.
#' @param windowSize length of the window to build each model. When \code{rolling == FALSE}, the each model will be
#' fit to a time series of this length, and when \code{rolling == TRUE} the first model will be fit to a series
#' of this length and grow by one each iteration.
#' @param maxHorizon maximum length of the forecast horizon to use for computing errors.
#' @param horizonAverage should the final errors be an average over all forecast horizons up to \code{maxHorizon} instead of producing
#' metrics for each individual horizon?
#' @param xreg External regressors to be used to fit the model. Only used if FUN accepts xreg as an argument. FCFUN is also expected to accept it (see details)
#' @param saveModels should the individual models be saved? Set this to \code{FALSE} on long time series to save memory.
#' @param saveForecasts should the individual forecast from each model be saved? Set this to \code{FALSE} on long time series to save memory.
#' @param verbose should the current progress be printed to the console?
#' @param num.cores the number of cores to use for parallel fitting. If the underlying model
#' that is being fit also utilizes parallelization, the number of cores it is using multiplied
#' by `num.cores` should not exceed the number of cores avaialble on your machine.
#' @param ... Other arguments to be passed to the model function FUN
#'
#' @details Cross validation of time series data is more complicated than regular k-folds or leave-one-out cross validation of datasets
#' without serial correlation since observations \eqn{x_t}{x[t]} and \eqn{x_{t+n}}{x[t+n]} are not independent. The \code{cvts()} function overcomes
#' this obstacle using two methods: 1) rolling cross validation where an initial training window is used along with a forecast horizon
#' and the initial window used for training grows by one observation each round until the training window and the forecast horizon capture the
#' entire series or 2) a non-rolling approach where a fixed training length is used that is shifted forward by the forecast horizon
#' after each iteration.
#'
#' For the rolling approach, training points are heavily recycled, both in terms of used for fitting
#' and in generating forecast errors at each of the forecast horizons from \code{1:maxHorizon}. In constrast, the models fit with
#' the non-rolling approach share less overlap, and the predicted forecast values are also only compared to the actual values once.
#' The former approach is similar to leave-one-out cross validation while the latter resembles k-fold cross validation. As a result,
#' rolling cross validation requires far more iterations and computationally takes longer to complete, but a disadvantage of the
#' non-rolling approach is the greater variance and general instability of cross-validated errors.
#'
#' The \code{FUN} and \code{FCFUN} arguments specify which function to use
#' for generating a model and forecasting, respectively. While the functions
#' from the "forecast" package can be used, user-defined functions can also
#' be tested, but the object returned by \code{FCFUN} must
#' accept the argument \code{h} and contain the point forecasts out to
#' this horizon \code{h} in slot \code{$mean} of the returned object. An example is given with
#' a custom model and forecast.
#'
#' For small time series (default \code{length <= 500}), all of the individual fit models are included in the final
#' \code{cvts} object that is returned. This can grow quite large since functions such as \code{auto.arima} will
#' save fitted values, residual values, summary statistics, coefficient matrices, etc. Setting \code{saveModels = FALSE}
#' can be safely done if there is no need to examine individual models fit at every stage of cross validation since the
#' forecasts from each fold and the associated residuals are always saved.
#'
#' External regressors are allowed via the \code{xreg} argument. It is assumed that both \code{FUN} and \code{FCFUN} accept the \code{xreg} parameter if \code{xreg} is not \code{NULL}.
#' If \code{FUN} does not accept the \code{xreg} parameter a warning will be given. No warning is provided if \code{FCFUN} does not use the \code{xreg} parameter.
#' @seealso \code{\link{accuracy.cvts}}
#'
#' @examples
#' cvmod1 <- cvts(AirPassengers, FUN = stlm,
#'                windowSize = 48, maxHorizon = 12)
#' accuracy(cvmod1)
#'
#' \dontrun{
#' cvmod2 <- cvts(USAccDeaths, FUN = ets,
#'                saveModels = FALSE, saveForecasts = FALSE,
#'                windowSize = 36, maxHorizon = 12)
#'
#' cvmod3 <- cvts(AirPassengers, FUN = hybridModel,
#'                FCFUN = forecast, rolling = TRUE, windowSize = 48,
#'                maxHorizon = 12)
#'
#' # We can also use custom functions, for example fcast()
#' from the "GMDH" package
#' library(GMDH)
#' GMDHForecast <- function(x, h){fcast(x, f.number = h)}
#' gmdhcv <- cvts(AirPassengers, FCFUN = GMDHForecast)
#' gmdhcv <- cvts(AirPassengers, FCFUN = GMDHForecast)
#'
#' # Example with custom model function and forecast function
#' customMod <- function(x){
#'  result <- list()
#'  result$series <- x
#'  result$last <- tail(x, n = 1)
#'  class(result) <- "customMod"
#'  return(result)
#' }
#' forecast.customMod <- function(x, h = 12){
#'  result <- list()
#'  result$model <- x
#'  result$mean <- rep(x$last, h)
#'  class(result) <- "forecast"
#'  return(result)
#' }
#' cvobj <- cvts(AirPassengers, FUN = customMod, FCFUN = forecast.customMod)
#'
#' # Use the rwf() function from the "forecast" package.
#' # This function does not have a modeling function and
#' # instead calculates a forecast on the time series directly
#' rwcv <- cvts(AirPassengers, FCFUN = rwf)
#' }
#'
#' @author David Shaub
#' from doParallel import registerDoParallel
#' from parallel import stopCluster
#' from foreach import foreach
cvts <- function(x, FUN = NULL, FCFUN = NULL,
                 rolling = FALSE, windowSize = 84,
                 maxHorizon = 5,
                 horizonAverage = FALSE,
                 xreg = NULL,
                 saveModels = ifelse(length(x) > 500, FALSE, TRUE),
                 saveForecasts = ifelse(length(x) > 500, FALSE, TRUE),
                 verbose = TRUE, num.cores = 1,
                 ...){
  # Default model function
  # This can be useful for methods that estimate the model and forecasts in one step
  # e.g. GMDH() from the "GMDH" package or thetaf()/meanf()/rwf() from "forecast". In this case,
  # no model function is entered but the forecast function is entered for
  # FCFUN
  if(is.null(FUN)){
    FUN <- function(x){
      return(x)
    }
  }
  # Default forecast function
  if(is.null(FCFUN)){
    FCFUN <- forecast
  }
  f = frequency(x)
  tspx <- tsp(x)
  if(is.null(tspx)){
    x <- ts(x, frequency = f)
  }

  if(any(sapply(c(x, windowSize, maxHorizon), FUN = function(x) !is.numeric(x)))){
    stop("The arguments x, windowSize, and maxHorizon must all be numeric.")
  }

  if(any(c(windowSize, maxHorizon) < 1L)){
    stop("The arguments windowSize, and maxHorizon must be positive integers.")
  }

  if(any(c(windowSize, maxHorizon) %% 1L != 0)){
    stop("The arguments windowSize, and maxHorizon must be positive integers.")
  }

  # Ensure at least two periods are tested
  if(windowSize + 2 * maxHorizon > length(x)){
    stop("The time series must be longer than windowSize + 2 * maxHorizon.")
  }

  # Check if fitting function accepts xreg when xreg is not NULL
  xregUse <- FALSE
  if (!is.null(xreg)) {
    fitArgs <- formals(FUN)
    if (any(grepl("xreg", names(fitArgs)))) {
      xregUse <- TRUE
      xreg <- as.matrix(xreg)
    } else
      warning("Ignoring xreg parameter since fitting function does not accept xreg")
  }

  # Combined code for rolling/nonrolling CV
  nrow = ifelse(rolling, length(x) - windowSize - maxHorizon + 1,
                as.integer((length(x) - windowSize) / maxHorizon))
  results <- matrix(NA, nrow = nrow, ncol = maxHorizon)

  forecasts <- fits <- vector("list", nrow(results))
  slices <- tsPartition(x, rolling, windowSize, maxHorizon)

  # Perform the cv fits
  # adapted from code from Rob Hyndman at http://robjhyndman.com/hyndsight/tscvexample/
  # licensend under >= GPL2 from the author

  cl <- parallel::makeCluster(num.cores)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl))
  # Appease R CMD CHECK with sliceNum declaration
  sliceNum <- NULL
  results <- foreach::foreach(sliceNum = seq_along(slices),
                              .packages = c("forecastHybrid", "forecast")) %dopar% {
    if(verbose){
      cat("Fitting fold", sliceNum, "of", nrow(results), "\n")
    }
    results <- list()

    trainIndices <- slices[[sliceNum]]$trainIndices
    testIndices <- slices[[sliceNum]]$testIndices

    tsTrain <- tsSubsetWithIndices(x, trainIndices)
    tsTest <- tsSubsetWithIndices(x, testIndices)

    if(xregUse){
      xregTrain <- xreg[trainIndices, ,drop = FALSE]
      xregTest <- xreg[testIndices, ,drop = FALSE]
      mod <- do.call(FUN, list(tsTrain, xreg = xregTrain, ...))
      fc <- do.call(FCFUN, list(mod, xreg = xregTest, h = maxHorizon))
    }else{
      mod <- do.call(FUN, list(tsTrain, ...))
      fc <- do.call(FCFUN, list(mod, h = maxHorizon))
    }

    if(saveModels){
      results$fits <- mod
    }

    if(saveForecasts){
      results$forecasts <- fc
    }

    results$resids <- tsTest - fc$mean
    results
  }

  # Gather the parallel chunks
  residlist <- lapply(results, function(x) unlist(x$resids))
  resids <- matrix(unlist(residlist, use.names = FALSE),
                   ncol = maxHorizon, byrow = TRUE)
  forecasts <- lapply(results, function(x) x$forecasts)
  fits <- lapply(results, function(x) x$fits)

  # Average the results from all forecast horizons up to maxHorizon
  if(horizonAverage){
    resids <- as.matrix(rowMeans(resids), ncol = 1)
  }

  if(!saveModels){
    fits <- NULL
  }
  if(!saveForecasts){
    forecasts <- NULL
  }

  params <- list(FUN = FUN,
                 FCFUN = FCFUN,
                 rolling = rolling,
                 windowSize = windowSize,
                 maxHorizon = maxHorizon,
                 horizonAverage = horizonAverage,
                 saveModels = saveModels,
                 saveForecasts = saveForecasts,
                 verbose = verbose,
                 num.cores = num.cores,
                 extra = list(...))

  result <- list(x = x,
               xreg = xreg,
               params = params,
               forecasts = forecasts, 
               models = fits, 
               residuals = resids)

  class(result) <- "cvts"
  return(result)
}

#' Generate training and test indices for time series cross validation
#' 
#' Training and test indices are generated for time series cross validation.
#' Generated indices are based on the training windowSize, forecast horizons
#' and whether a rolling or non-rolling cross validation procedure is desired.
#' 
#' @export 
#' @param x A time series
#' @param rolling Should indices be generated for a rolling or non-rolling procedure?
#' @param windowSize Size of window for training
#' @param maxHorizon Maximum forecast horizon
#' 
#' @return List containing train and test indices for each fold
#' 
#' @author Ganesh Krishnan
#' @examples 
#' \dontrun{
#' tsPartition(AirPassengers, rolling = TRUE, windowSize = 10, maxHorizon = 2)
#' }

tsPartition <- function(x, rolling, windowSize, maxHorizon) {
  numPartitions <- ifelse(rolling, length(x) - windowSize - maxHorizon + 1, as.integer((length(x) - windowSize) / maxHorizon))

  slices <- rep(list(NA), numPartitions)
  start <- 1

    for (i in 1:numPartitions) {
        if(rolling){
            trainIndices <- seq(start, start + windowSize - 1, 1)
            testIndices <-  seq(start + windowSize, start + windowSize + maxHorizon - 1)
            start <- start + 1
        }
        ## Sample the correct slice for nonrolling
        else{
            trainIndices <- seq(start, start + windowSize - 1 + maxHorizon * (i - 1), 1)
            testIndices <- seq(start + windowSize + maxHorizon * (i - 1), start + windowSize - 1 + maxHorizon * i)
        }

        slices[[i]] <- list(trainIndices = trainIndices, testIndices = testIndices)
    }

    return(slices)
}

#' Extract cross validated rolling forecasts
#' 
#' Obtain cross validated forecasts when rolling cross validation is used. The object is not
#' inspected to see if it was fit using a rolling origin
#' 
#' @export 
#' @param cv An object of class cvts
#' @param horizon The forecast horizon from each fold to extract
#' 
#' @return Forecasts computed via a rolling origin
#' 
#' @details Combine the cross validated forecasts fit with a rolling origin. This may be useful
#' to visualize and investigate the cross validated performance of the model
#' 
#' @author Ganesh Krishnan
#' @examples 
#' \dontrun{
#' cv <- cvts(AirPassengers, FUN = "ets", FCFUN = "forecast", 
#'         rolling = TRUE, windowSize = 12, horizon = 2)
#' 
#' extractRollingForecasts(cv)
#' }
extractForecasts <- function(cv, horizon = 1) {
      if (horizon > cv$params$maxHorizon) 
         stop("Cannot extract forecasts with a horizon greater than the model maxHorizon")
      pointfList <- Map(function(fcast) {
         pointf <- fcast$mean
         window(pointf, start = time(pointf)[horizon], 
                                   end = time(pointf)[horizon])
         }, 
         cv$forecasts) 

      pointf <- Reduce(tsCombine, pointfList)

      #Ensure all points in the original series are represented (makes it easy for comparisons)
      template <- replace(cv$x, c(1:length(cv$x)), NA)
      return(tsCombine(pointf, template))
}
