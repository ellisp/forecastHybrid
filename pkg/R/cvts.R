#' Cross validation for time series
#'
#' Perform cross validation on time series 
#'
#' @export
#' @param x the input time series
#' @param FUN the model function used. See details.
#' @param FCFUN a function that proces point forecasts for the model function. This defaults to \code{\link{forecast()}}.
#' See details.
#' @param rolling should a rolling procedure be used? If TRUE, nonoverlapping windows of size \code{maxHorizon}
#' will be used for fitting each model. If FALSE, the size of the dataset used for training will grow
#' by one each iteration.
#' @param windoSize length of the window to build each model. When \code{rolling == TRUE}, the each model will be
#' fit to a time series of this length, and when \code{rolling == FALSE} the first model will be fit to a series
#' of this length and grow by one each iteration
#' @param maxHorizon maximum length of the forecast horizon to use for computing errors
#' @param horizonAverage should the final errors be an average over all forecast horizons up to \code{maxHorizon} instead of producing
#' metrics for each individual horizon?
#' @param verbose should the current progress be printed to the console?
#' 
#' @details Cross validation of time series is more complicated than regular k-folds of leave one out cross validation of datasets
#' without serial correlation since observations \eqn{x_t}{x[t]} and \eqn{x_{t+n}}{x[t+n]} are not independent. The \code{cvts()} function overcomes
#' this obstacle using two methods: rolling cross validation where an initial training window is used along with a forecast horizon
#' and the initial window used for training grows by one each round until the training window and the forecast horizon capture the
#' entire series or a nonrolling approach where a fixed training horizon is used that is shifted forward by the forecast horizon
#' after each iteration.
#' \cr
#' For the rolling approach, training points are heavily recycled, both in terms of used for fitting
#' and in generating forecast errors at each of the forecast horizons from \code{1:maxHorizon}. In constrast, the models fit with
#' the nonrolling approach share less overlap, and the predicted forecast values are also only compared to the actual values once.
#' former approach is similar to leave one out cross validatoin whlie the latter resembles k-fold cross validation. As a result,
#' rolling cross validation can use far more iterations and computationally take longer to complete, but a disadvantage of the
#' nonrolling approach is the greater variance and general instability of cross validated errors.
#' 
#' 
#' @examples
#' 
#' cvmod1 <- cvts(AirPassengers, FUN = ets, FCFUN = forecast, rolling = TRUE, windowSize = 48, maxHorizon = 12)
#' cvmod2 <- cvts(AirPassengers, FUN = hybridModel, FCFUN = forecast, rolling = TRUE, windowSize = 48, maxHorizon = 12)
cvts <- function(x, FUN = NULL, FCFUN = NULL,
                 rolling = FALSE, windowSize = 84,
                 useHorizon = 5, maxHorizon = 5,
                 horizonAverage = FALSE,
                 verbose = TRUE){
   # Default forecast function
   if(is.null(FCFUN)){
      FCFUN <- forecast
   }
   x <- ts(x)
   f <- frequency(x)
   if(any(sapply(c(x, windowSize, useHorizon, maxHorizon), FUN = function(x) !is.numeric(x)))){
     stop("The arguments x, windowSize, useHorizon, and maxHorizon must all be numeric.")
   }
   
   
   if(any(c(windowSize, useHorizon, maxHorizon) < 1L)){
     stop("The arguments windowSize, useHorizon, and maxHorizon must be positive integers.")
   }
   
   if(any(c(windowSize, useHorizon, maxHorizon) %% 1L != 0)){
     stop("The arguments windowSize, useHorizon, and maxHorizon must be positive integers.")
   }
   
   # Ensure at least two periods are tested
   if(windowSize + 2 * maxHorizon > length(x)){
     stop("The time series must be longer than windowSize + 2 * maxHorizon.")
   }
   
   # Combined code for rolling/nonrolling CV
   if(rolling){
     results <- matrix(NA,
                       nrow = length(x) - windowSize - maxHorizon,
                       ncol = maxHorizon)
   }
   else{
     results <- matrix(NA,
                       nrow = as.integer((length(x) - windowSize) / maxHorizon),
                       ncol = maxHorizon)
   }
   forecasts <- fits <- vector("list", nrow(results))
   
   # Needed for nonrolling
   startWindow <- 1
   endWindow <- windowSize
   # Perform the cv fits
   for(i in 1:nrow(results)){
     if(verbose){
       print(paste("Fitting fold", i, "of", nrow(results)))
     }
     # Sample the correct slice for rolling
     if(rolling){
       stsp <- tsp(x)[1]
       etsp <- stsp + (i + maxHorizon - 2) / frequency(x)
       y <- window(x, start = stsp, end = etsp)
       nextHorizon <- windowSize + maxHorizon
       ynext <- x[(windowSize + 1):nextHorizon]
       windowSize <- windowSize + 1
     }
     # Sample the correct slice for nonrolling
     else{
       stsp <- tsp(x)[1] + (i - 1) / frequency(x)
       etsp <- stsp + (maxHorizon - 1) / frequency(x)
       y <- window(x, start = stsp, end = etsp) 
       ynext <- x[(endWindow + 1):(endWindow + maxHorizon)]
       startWindow <- startWindow + maxHorizon
       endWindow <- endWindow + maxHorizon
     }
     # Perfom the simulation
     mod <- do.call(FUN, list(y))
     fits[[i]] <- mod
     fc <- do.call(FCFUN, list(mod, h = maxHorizon))
     forecasts[[i]] <- fc
     results[i, ] <- ynext - fc$mean
   }
   # Average the results from all forecast horizons up to maxHorizon
   if(horizonAverage){
      results <- rowMeans(results)
   }
   
   result <- list(forecasts = forecasts, models = fits, residuals = results)
   class(result) <- "cv"
   return(result)
}
