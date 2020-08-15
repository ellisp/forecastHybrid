#' Combine multiple sequential time series
#'
#' Combine multiple ts objects into a single ts object. It is assumed that the ts objects provided
#' are sequential. In other words, it is assumed that a valid time series object can actually
#' be constructed from the provided objects. The start time and frequency of the combined object
#' will correspond to the start time and frequency of the first provided object
#'
#' @export
#' @param ... ts objects to combine
#'
#' @return A combined ts object generated from the individual ts objects
#'
#' @details Combine sequential time series objects into a single time series object. This might
#' be useful, for example, when you want to combine the training and validation time series objects
#' for plotting. The function assumes that the provided objects have no overlap.
#' For example, a valid argument set would have two time series with periods from Jan-Dec 2015
#' and Jan-Dec 2016. An invalid set would be two time series t1 and t2 with periods from
#' Jan-Dec 2015 and Aug 2015-Dec 2016 respectively. In that case, there is overlap between
#' t1 and t2. The return value will depend on the order in which the arguments are provided.
#' If the function call is tsCombine(t1, t2), the overlapping portion of t1 and t2
#' (Aug-Dec 2015 in this example), would have values from t1 as long as they are not NA.
#' If the call is tsCombine(t2, t1), it will have values from t2 as long as they are not NA.
#'
#' @author Ganesh Krishnan
#' @examples
#' tsCombine(window(AirPassengers, end = c(1951, 12)), window(AirPassengers, start = c(1952, 1)))
tsCombine <- function(...) {
  chkDots(...)
  combinedDf <- ts.union(..., dframe = TRUE)
  combinedTs <- ts.union(..., dframe = FALSE)
  coalesced <- apply(combinedDf, 1,
                     function(x) ifelse(all(is.na(x)), NA, x[min(which(!is.na(x)))]))
  ret <- ts(coalesced, start = start(combinedTs), frequency = frequency(combinedTs))
  return(ret)
}

#' Subset time series with provided indices
#'
#' Use provided indices to subset a time series. The provided indices must be contiguous
#'
#' @export
#' @param x A time series object
#' @param indices A contiguous vector of indices to use for subsetting
#' @return A time series object appropriately subsetted using provided indices
#'
#' @author Ganesh Krishnan
#' @examples
#' tsSubsetWithIndices(AirPassengers, c(3:10))
tsSubsetWithIndices <- function(x,
                                indices) {
  xtime <- time(x)
  minIndex <- min(indices)
  maxIndex <- max(indices)

  if (maxIndex > length(xtime)) {
    stop("Max subset index cannot exceed time series length")
  }
  if (all(seq(minIndex, maxIndex, 1) != indices)) {
    stop("Time series can only be subset with continuous indices")
  }
  return(window(x, start = xtime[minIndex], end = xtime[maxIndex]))
}

#' Return a forecast model function for a given model character
#'
#' Convert the single-letter representation used in the "forecastHybrid" package to the
#' corresponding model function from the "forecast" package
#' @param modelCharacter a single character representing one of the models from the \code{models}
#' argument passed to \link{hybridModel}
#' @examples
#' forecastHybrid:::getModel("a")
#' forecastHybrid:::getModel("s")
#' forecastHybrid:::getModel("z")
#' @seealso \code{\link{hybridModel}}
getModel <- function(modelCharacter) {
  models <- c("a" = auto.arima, "e" = ets, "f" = thetam, "n" = nnetar,
              "s" = stlm, "t" = tbats, "z" = snaive)
  return(models[[modelCharacter]])
}

#' Translate character to model name
#'
#' Convert the single-letter representation used in the "forecastHybrid" package to the
#' corresponding function name from the "forecast" package
#' @param modelCharacter a single character representing one of the models from the \code{models}
#' argument passed to \link{hybridModel}
#' @examples
#' forecastHybrid:::getModelName("a")
#' forecastHybrid:::getModelName("s")
#' forecastHybrid:::getModelName("z")
#' @seealso \code{\link{hybridModel}}
getModelName <- function(modelCharacter) {
  models <- c("a" = "auto.arima", "e" = "ets", "f" = "thetam", "n" = "nnetar",
              "s" = "stlm", "t" = "tbats", "z" = "snaive")
  return(as.character(models[modelCharacter]))
}

#' Helper function used to unpack the fitted model objects from a list
#'
#' @param fitModels A list containing the models to include in the ensemble
#' @param expandedModels A character vector from the \code{models} argument of \link{hybridModel}
#' @details See usage inside the \code{hybridModel} function.
#' @seealso \code{\link{hybridModel}}
unwrapParallelModels <- function(fitModels,
                                 expandedModels) {
  modelResults <- list()
  for (i in seq_along(expandedModels)) {
    model <- expandedModels[i]
    modelResults[[getModelName(model)]] <- fitModels[[i]]
  }
  return(modelResults)
}

#' Helper function to remove models that require more data
#'
#' @param y The input time series
#' @param models The model codes to test
removeModels <- function(y, models) {
  expandedModels <- unique(models)
  # All characters must be valid
  validModels <- c("a", "e", "f", "n", "s", "t", "z")
  if (!all(expandedModels %in% validModels)) {
    stop("Invalid models specified.")
  }
  if (!length(expandedModels)) {
    stop("At least one component model type must be specified.")
  }

  # Check for problems for specific models (e.g. long seasonality for ets and non-seasonal
  # for stlm or nnetar)
  if (is.element("e", expandedModels) && frequency(y) > 24) {
    warning("frequency(y) > 24. The ets model will not be used.")
    expandedModels <- expandedModels[expandedModels != "e"]
  }
  if (is.element("f", expandedModels) && length(y) <= frequency(y)) {
    warning("The theta model requires more than one seasonal period of data.",
            " The theta model will not be used.")
    expandedModels <- expandedModels[expandedModels != "f"]
  }

  if (is.element("s", expandedModels)) {
    if (frequency(y) < 2L) {
      warning("The stlm model requires that the input data be a seasonal ts object.",
              " The stlm model will not be used.")
      expandedModels <- expandedModels[expandedModels != "s"]
    }
    if (frequency(y) * 2L >= length(y)) {
      warning("The stlm model requres a series more than twice as long as the seasonal period.",
              " The stlm model will not be used.")
      expandedModels <- expandedModels[expandedModels != "s"]
    }
  }
  if (is.element("n", expandedModels)) {
    if (frequency(y) * 2L >= length(y)) {
      warning("The nnetar model requres a series more than twice as long as the seasonal period.",
              " The nnetar model will not be used.")
      expandedModels <- expandedModels[expandedModels != "n"]
    }
  }
  # A model run should include at least two component models
  if (length(expandedModels) < 2L) {
    stop("A hybridModel must contain at least two component models.")
  }
  return(expandedModels)
}

#' Helper function to check the that the parallel arguments are valid
#'
#' @param parallel A logic to indicate if parallel processing should be used
#' @param num.cores An integer for the number of threads to use
checkParallelArguments <- function(parallel,
                                   num.cores) { # nolint
  # Validate cores and parallel arguments
  if (!is.logical(parallel)) {
    stop("The parallel argument must be TRUE/FALSE.")
  }
  if (!is.numeric(num.cores)) {
    stop("The number of cores specified must be an integer greater than zero.")
  }
  if (as.logical((num.cores %% 1L)) || num.cores <= 0L) {
    stop("The number of cores specified must be an integer greater than zero.")
  }
}

#' Helper function to validate and clean the input time series
#'
#' @param y The input time series
prepareTimeseries <- function(y) {
  # The dependent variable must be numeric and not a matrix/dataframe
  forbiddenTypes <- c("data.frame", "data.table", "matrix")
  if (!is.numeric(y) || all(class(y) %in% forbiddenTypes)) {
    stop("The time series must be numeric and may not be a matrix or dataframe object.")
  }
  if (!length(y)) {
    stop("The time series must have observations")
  }

  if (length(y) < 1) {
    stop("The time series must have an observations")
  }
  y <- as.ts(y)

  return(y)
}

#' Helper function to test all the model arguments (e.g. a.args, e.args, etc)
#'
#' @param modelArguments A list of containing the model arguments
#' @param models A character vector containing all the model codes
checkModelArgs <- function(modelArguments,
                           models) {
  expandedModels <- models
  for (i in seq_along(modelArguments)) {
    modelCode <- names(modelArguments)[i]
    currentArg <- modelArguments[[i]]
    if (!is.null(currentArg) && !is.element(modelCode, expandedModels)) {
      argsName <- paste0(modelCode, ".args")
      warning(getModelName(modelCode), " was not selected in the models argument, but",
              argsName, " was passed. Ignoring ", argsName)
    }
  }
}
