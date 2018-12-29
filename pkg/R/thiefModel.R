#' Forecast ensemble using THieF
#'
#' Create a forecast ensemble using the theif() model


#' @details Use the "thief" package method for reconciling forecasts across the temporal hierarchy.
#' The base models to be included in the ensemble are the same as those in \text{hybridModel}, but
#' the \code{stlm} model cannot be included since it requires seasonal data.
#'
#' @author David Shaub
#' @seealso \code{\link{[thief]thief}}
#' @seealso \code{\link{hybridModel}}
#'
thiefModel <- function(y, models = "aefnt", h = 2 * frequency(x), verbose = TRUE){
  forecasts <- list()
  models <- unlist(strsplit(models, ""))
  for(modelChar in models){
    if(verbose){
      message("Fitting the ", modelName <- getModelName(modelChar), " model")
    }
    FUN <- getModel(modelChar)
    FCFUN <- function(y, h) forecast(FUN(y), h = h)
    fc <- thief(y, h = h, forecastfunction = FCFUN)
    forecasts[[modelName]] <- fc
  }


  ##############################################################################
  # Prepare forecast object
  ##############################################################################


  forecasts$mean <- NULL
  forecasts$x <- y
  forecasts$fitted <- forecasts$residuals <- NULL
  forecasts$method <- "THieF-FCFUN"
  class(forecasts) <- "forecast"
  return(forecasts)
}
