#' Forecast ensemble using THieF
#'
#' Create a forecast ensemble using the theif() model
#'
#' @export
#' @param y the input time series
#' @param models the models to use. These are specified the same way as \code{\link{hybridModel}}
#' @param h the forecast horizon
#' @param comb the combination method to use by \code{\link[thief]{thief}}
#' @param verbose if \code{TRUE}, report the fitting status
#' @details Use the "thief" package method for reconciling forecasts across the temporal hierarchy.
#' The base models to be included in the ensemble are the same as those in \code{hybridModel}, but
#' the \code{stlm} model cannot be included since it requires seasonal data.
#'
#' @examples
#' series <- subset(woolyrnq, end = 8)
#' thiefModel(series, models = "fz")
#' @author David Shaub
#' @seealso \code{\link[thief]{thief}}
#' @seealso \code{\link{hybridModel}}
#'
thiefModel <- function(y,
                       models = "aefnt",
                       h = 2 * frequency(y),
                       comb=c("struc", "mse", "ols", "bu", "shr", "sam"),
                       verbose = FALSE) {

  ##############################################################################
  # Validate input
  ##############################################################################
  # Validate and clean the input timeseries
  y <- prepareTimeseries(y = y)
  # Match the specified models
  # Note: models that only work with seasonal data (e.g. stlm()) will not work
  models <- sort(unique(tolower(unlist(strsplit(models, split = "")))))
  # Check models and data length to ensure enough data: remove models that
  # require more data
  models <- removeModels(y = y, models = models)

  comb <- match.arg(comb)

  ##############################################################################
  # Fit models
  ##############################################################################


  forecasts <- list()
  fcTsp <- NULL
  for (modelChar in models) {
    modelName <- getModelName(modelChar)
    if (verbose) {
      message("Fitting the ", modelName <- getModelName(modelChar), " model")
    }
    FUN <- getModel(modelChar) # nolint
    FCFUN <- function(y1, h1) forecast(object = FUN(y = y1), h = h1) # nolint
    fc <- thief(y = y, h = h, comb = comb, forecastfunction = FCFUN)
    forecasts[[modelName]] <- fc
    fcTsp <- tsp(fc$mean)
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
  tsp(fc) <- fcTsp

  forecasts$mean <- fc
  forecasts$x <- y
  forecasts$fitted <- fit
  forecasts$residuals <- y - fit
  forecasts$method <- "THieF-FCFUN"
  class(forecasts) <- "forecast"
  return(forecasts)
}
