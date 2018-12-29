#' Forecast ensemble using THieF
#'
#' Create a forecast ensemble using the theif() model
#'
#' @param y the input time series
#' @param models the models to use. These are specified the same way as \code{\link{hybridModel}}
#' @param h the forecast horizon
#' @param verbose if \code{TRUE}, report the fitting status
#' @details Use the "thief" package method for reconciling forecasts across the temporal hierarchy.
#' The base models to be included in the ensemble are the same as those in \code{hybridModel}, but
#' the \code{stlm} model cannot be included since it requires seasonal data.
#'
#' @examples
#' thiefModel(USAccDeaths, models = "ef")
#' @author David Shaub
#' @seealso \code{\link{[thief]{thief}}}
#' @seealso \code{\link{hybridModel}}
#'
thiefModel <- function(y, models = "aefnt", h = 2 * frequency(x), verbose = FALSE){
  forecasts <- list()
  models <- unlist(strsplit(models, ""))
  fc_tsp <- NULL
  for(modelChar in models){
    if(verbose){
      message("Fitting the ", modelName <- getModelName(modelChar), " model")
    }
    FUN <- getModel(modelChar)
    FCFUN <- function(y, h) forecast(FUN(y = y), h = h)
    fc <- thief(y, h = h, forecastfunction = FCFUN)
    fit <- fitted(fc)
    forecasts[[modelName]] <- fc
    fc_tsp <- tsp(fc$mean)
  }


  ##############################################################################
  # Prepare forecast object
  ##############################################################################


  fits <- sapply(forecasts, FUN = function(x) fitted(x))
  weights <- rep(1 / length(models), length(models)) # equal weights for now
  weightMatrix <- matrix(rep(weights, times = nrow(fits)),
                         nrow = nrow(fits), byrow = TRUE)
  fit <- rowSums(fits * weightMatrix)

  weightMatrix <- matrix(rep(weights, times = h),
                         nrow = h, byrow = TRUE)
  fc <- sapply(forecasts, FUN = function(x) x$mean)
  fc <- ts(rowSums(fc * weightMatrix))
  tsp(fc) <- fc_tsp

  forecasts$mean <- fc
  forecasts$x <- y
  forecasts$fitted <- fit
  forecasts$residuals <- y - fit
  forecasts$method <- "THieF-FCFUN"
  class(forecasts) <- "forecast"
  return(forecasts)
}

fc <- thiefModel(AirPassengers)
