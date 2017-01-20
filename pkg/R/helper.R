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
#' for plotting. The function assumes that the provided objects refer to a sequential time.
#' For example, a valid argument set would have two time series with periods from Jan-Dec 2015
#' and Jan-Dec 2016. An invalid set would be two time series with periods from Jan-Dec 2015
#' and Feb-Dec 2016. No sanity checks are performed, so it is the caller's responsiblity to ensure
#' that the time series are sequential
#' 
#' @examples 
#' tsCombine(window(AirPassengers, end = c(1951, 12)), window(AirPassengers, start = c(1952, 1)))
tsCombine <- function(...) {
   ts.union(..., dframe = TRUE) %>%
      apply(1, function(x) x[min(which(!is.na(x)))]) %>%
      ts(start = start(...), frequency = frequency(...))
}