#' Hybrid time series modeling
#'
#' Create a hybrid time series model with two to five component models.
#'
#' @export
#' @import forecast
#' @import thief
#' @import stats
#' @import graphics
#' @import zoo
#' @param y A numeric vector or time series.
#' @param lambda
#' Box-Cox transformation parameter.
#' Ignored if NULL. Otherwise, data transformed before model is estimated.
#' @param models A character string of up to seven characters indicating which contributing models to use:
#' a (\code{\link[forecast]{auto.arima}}), e (\code{\link[forecast]{ets}}),
#' f (\code{\link{thetam}}), n (\code{\link[forecast]{nnetar}}),
#' s (\code{\link[forecast]{stlm}}), t (\code{\link[forecast]{tbats}}), and z (\code{\link[forecast]{snaive}}).
#' @param a.args an optional \code{list} of arguments to pass to \code{\link[forecast]{auto.arima}}. See details.
#' @param e.args an optional \code{list} of arguments to pass to \code{\link[forecast]{ets}}. See details.
#' @param n.args an optional \code{list} of arguments to pass to \code{\link[forecast]{nnetar}}. See details.
#' @param s.args an optional \code{list} of arguments to pass to \code{\link[forecast]{stlm}}. See details.
#' @param t.args an optional \code{list} of arguments to pass to \code{\link[forecast]{tbats}}. See details.
#' @param z.args an optional \code{list} of arguments to pass to \code{\link[forecast]{snaive}}. See details.
#' @param weights method for weighting the forecasts of the various contributing
#' models.  Defaults to \code{equal}, which has shown to be robust and better
#' in many cases than giving more weight to models with better in-sample performance. Cross validated errors--implemented with \code{link{cvts}}
#' should produce the best forecast, but the model estimation is also the slowest. Note that extra arguments
#' passed in \code{a.args}, \code{e.args}, \code{n.args}, \code{s.args}, and \code{t.args} are not used
#' during cross validation. See further explanation in \code{\link{cvts}}.
#' Weights utilizing in-sample errors are also available but not recommended.
#' @param errorMethod  method of measuring accuracy to use if weights are not
#' to be equal.
#' Root mean square error (\code{RMSE}), mean absolute error (\code{MAE})
#' and mean absolute scaled error (\code{MASE})
#' are supported.
#' @param parallel a boolean indicating if parallel processing should be used between models.
#' Parallelization will still occur within individual models that support it and can be controlled using \code{a.args} and \code{t.args}.
#' @param num.cores If \code{parallel=TRUE}, how many cores to use.
#' @param cvHorizon If \code{weights = "cv.errors"}, this controls which forecast to horizon to use
#' for the error calculations.
#' @param windowSize length of the window to build each model, only used when \code{weights = "cv.errors"}.
#' @param horizonAverage If \code{weights = "cv.errors"}, setting this to \code{TRUE} will average
#' all forecast horizons up to \code{cvHorizon} for calculating the errors instead of using
#' the single horizon given in \code{cvHorizon}.
#' @param verbose Should the status of which model is being fit/cross validated be printed to the terminal?
#' @seealso \code{\link{forecast.hybridModel}}, \code{\link[forecast]{auto.arima}},
#' \code{\link[forecast]{ets}}, \code{\link{thetam}}, \code{\link[forecast]{nnetar}},
#' \code{\link[forecast]{stlm}}, \code{\link[forecast]{tbats}}
#' @return An object of class hybridModel.
#' The individual component models are stored inside of the object
#' and can be accessed for all the regular manipulations available in the forecast package.
#' @details The \code{hybridModel} function fits multiple individual model specifications to allow easy creation
#' of ensemble forecasts. While default settings for the individual component models work quite well
#' in most cases, fine control can be exerted by passing detailed arguments to the component models in the
#' \code{a.args}, \code{e.args}, \code{n.args}, \code{s.args}, and \code{t.args} lists.
#' Note that if \code{xreg} is passed to the \code{a.args}, \code{n.args}, or \code{s.args} component models
#' it must now be passed as a matrix. In "forecastHybrid" versions earlier than 4.0.15 it would
#' instead be passed in as a dataframe, but for consistency with "forecast" v8.5 we now require
#' a matrix with colnames
#' \cr
#' \cr
#' Characteristics of the input series can cause problems for certain types of models and parameters.
#' For example, \code{\link[forecast]{stlm}} models require that the input series be seasonal;
#' furthermore, the data must include at least two seasons of data (i.e. \code{length(y) >= 2 * frequency(y)})
#' for the decomposition to succeed.
#' If this is not the case, \code{hybridModel()}
#' will remove the \code{stlm} model so an error does not occur.
#' Similarly, \code{nnetar} models require that
#' \code{length(y) >= 2 * frequency(y)}, so these models will be removed if the condition is not satisfied
#' The \code{\link[forecast]{ets}} model does not handle
#' a series well with a seasonal period longer than 24 and will ignore the seasonality. In this case,
#' \code{hybridModel()} will also drop the \code{ets} model from the ensemble.
#'
#' @examples
#' \dontrun{
#'
#' # Fit an auto.arima, ets, thetam, nnetar, stlm, and tbats model
#' # on the time series with equal weights
#' mod1 <- hybridModel(AirPassengers)
#' plot(forecast(mod1))
#'
#' # Use an auto.arima, ets, and tbats model with weights
#' # set by the MASE in-sample errors
#' mod2 <- hybridModel(AirPassengers, models = "aet",
#' weights = "insample.errors", errorMethod = "MASE")
#'
#' # Pass additional arguments to auto.arima() to control its fit
#' mod3 <- hybridModel(AirPassengers, models = "aens",
#' a.args = list(max.p = 7, max.q = 7, approximation = FALSE))
#'
#' # View the component auto.arima() and stlm() models
#' mod3$auto.arima
#' mod3$stlm
#' }
#'
#' @author David Shaub
#'
hybridModel <- function(y, models = "aefnst",
                        lambda = NULL,
                        a.args = NULL,
                        e.args = NULL,
                        n.args = NULL,
                        s.args = NULL,
                        t.args = NULL,
                        z.args = NULL,
                        weights = c("equal", "insample.errors", "cv.errors"),
                        errorMethod = c("RMSE", "MAE", "MASE"),
                        cvHorizon = frequency(y),
                        windowSize = 84,
                        horizonAverage = FALSE,
                        parallel = FALSE, num.cores = 2L,
                        verbose = TRUE){

  ##############################################################################
  # Validate input
  ##############################################################################
  modelArguments = list("a" = a.args, "e" = e.args, "f" = NULL, "n" = n.args,
                    "s" = s.args, "t" = t.args, "z" = z.args)

  # Validate and clean the input timeseries
  y <- prepareTimeseries(y = y)

  # using weights = "insample.errors" will tend to overfit and favor complex models, so it is
  # not recommended
  weights <- match.arg(weights)
  if(weights == "insample.errors"){
    wrnMsg <- paste0("Using insample.error weights is not recommended for accuracy and ",
                     "may be deprecated in the future.")
    warning(wrnMsg)
  }
  errorMethod <- match.arg(errorMethod)

  # Match the specified models
  expandedModels <- sort(unique(tolower(unlist(strsplit(models, split = "")))))
  # Check models and data length to ensure enough data: remove models that require more data
  expandedModels <- removeModels(y = y, models = expandedModels)

  # Check the parallel arguments
  checkParallelArguments(parallel = parallel, num.cores = num.cores)

  # Check a.args/t.args/e.args/n.args/s.args
  checkModelArgs(modelArguments = modelArguments, models = expandedModels)

  if(weights == "cv.errors" && errorMethod == "MASE"){
    warning("cv errors currently do not support MASE. Reverting to RMSE.")
    errorMethod <- "RMSE"
  }

  ##############################################################################
  # Fit models
  ##############################################################################

  # Parallel execuation
  if(parallel){
    if(.Platform$OS.type == "unix"){
      cl <- parallel::makeForkCluster(num.cores)
    } else{
      cl <- parallel::makeCluster(num.cores)
    }
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    # Parallel processing won't be used by default since the benefit only occurs on long series
    # with large frequency
    currentModel <- NULL
    fitModels <- foreach::foreach(modelCode = expandedModels,
                                  .packages = c("forecast", "forecastHybrid")) %dopar% {
      # thetam() currently does not handle arguments
      if(modelCode == "f"){
         fitModel <- thetam(y)
      } else{ # All other models handle lambda and additional arguments
        argsAdditional <- modelArguments[[modelCode]]
        if(is.null(argsAdditional)){
          argsAdditional <- list(lambda = lambda)
        } else if(is.null(argsAdditional$lambda)){
          argsAdditional$lambda <- lambda
        }
        currentModel <- purrr::partial(getModel(modelCode), y = y)
        fitModel <- do.call(currentModel, argsAdditional)
      }
      fitModel
    }
    modelResults <- unwrapParallelModels(fitModels, expandedModels)
  } else{# serial execution
    modelResults <- list()
    for(modelCode in expandedModels){
      modelName <- getModelName(modelCode)
      if(verbose){
        message("Fitting the ", modelName, " model")
      }
      # thetam() currently does not handle arguments
      if(modelCode == "f"){
         modelResults[[modelName]] <- thetam(y)
      } else{ # All other models handle lambda and additional arguments
        argsAdditional <- modelArguments[[modelCode]]
        if(is.null(argsAdditional)){
          argsAdditional <- list(lambda = lambda)
        } else if(is.null(argsAdditional$lambda)){
          argsAdditional$lambda <- lambda
        }
        currentModel <- purrr::partial(getModel(modelCode), y = y)
        modelResults[[modelName]] <- do.call(currentModel, argsAdditional)
      }
    }
  }

  ##############################################################################
  # Determine model weights
  ##############################################################################

  # Set the model weights
  includedModels <- names(modelResults)
  numModels <- length(expandedModels)
  if(weights == "equal"){
    modelResults$weights <- rep(1 / numModels, numModels)
  } else if(weights %in% c("insample.errors", "cv.errors")){
    modelResults$weights <- rep(0, numModels)
    modResults <- modelResults

    if(weights == "cv.errors"){
      for(modelCode in expandedModels){
        modelName <- getModelName(modelCode)
        currentModel <- getModel(modelCode)
        if(verbose){
          message("Cross validating the ", modelName, " model")
        }
        modResults[[modelName]] <- cvts(y, FUN = currentModel,
                                        maxHorizon = cvHorizon,
                                        horizonAverage = horizonAverage,
                                        verbose = FALSE,
                                        windowSize = windowSize,
                                        num.cores = num.cores)
      }
    }
    # If horizonAverage == TRUE, the resulting accuracy object will have only one row
    cvHorizon <- ifelse(horizonAverage, 1, cvHorizon)
    cvHorizon <- ifelse(weights != "cv.errors", 1, cvHorizon)

    # Set the weights
    modelResults$weights <- sapply(expandedModels,
                                   function(x) accuracy(modResults[[getModelName(x)]])[cvHorizon,
                                                                                       errorMethod])

    # Scale the weights
    inverseErrors <- 1 / modelResults$weights
    modelResults$weights <- inverseErrors / sum(inverseErrors)

  }

  # Check for valid weights when weights = "insample.errors" and submodels produce perfect fits
  if(is.element(NaN, modelResults$weights) & weights %in% c("insample.errors", "cv.errors")){
    wrnMsg <- paste0("At least one model perfectly fit the series, so accuracy measures cannot",
                     " be used for weights. Reverting to weights = \"equal\".")
    warning(wrnMsg)
    modelResults$weights <- rep(1/ length(includedModels),
                                length(includedModels))
  }
  names(modelResults$weights) <- includedModels

  ##############################################################################
  # Prepare hybridModel object
  ##############################################################################

  # Apply the weights to construct the fitted values
  fits <- sapply(includedModels, FUN = function(x) fitted(modelResults[[x]]))
  fitsWeightsMatrix <- matrix(rep(modelResults$weights[includedModels],
                              times = nrow(fits)),
                              nrow = nrow(fits), byrow = TRUE)
  fits <- rowSums(fits * fitsWeightsMatrix)
  resid <- y - fits
  # If y is a ts, make fits and resid a ts too
  if (!is.null(tsp(y))){
    fits <- ts(fits)
    resid <- ts(fits)
    tsp(fits) <- tsp(resid) <- tsp(y)
  }

  # Save which models used xreg
  xregs <- list()
  if("a" %in% expandedModels){
    xregs$auto.arima <- ifelse("xreg" %in% names(a.args) && !is.null(a.args$xreg), TRUE, FALSE)
  }
  if("n" %in% expandedModels){
    xregs$nnetar <- ifelse("xreg" %in% names(n.args) && !is.null(n.args$xreg), TRUE, FALSE)
  }
  if("s" %in% expandedModels){
    methodArima <- "method" %in% names(s.args) && s.args$method == "arima"
    xregs$stlm <- ifelse("xreg" %in% names(s.args) && !is.null(s.args$xreg) && methodArima,
                         TRUE, FALSE)
  }

  # Prepare the hybridModel object
  class(modelResults) <- "hybridModel"
  modelResults$frequency <- frequency(y)
  modelResults$x <- y
  modelResults$xreg <- xregs
  modelResults$models <- includedModels
  modelResults$fitted <- fits
  modelResults$residuals <- resid
  return(modelResults)
}
