#' Cross validatoin for time series
#'
#' Perform cross validation on time series 
#'
#' @export
#' @param data the input time series
#' @param FUN the model function.
#' @param FCFUN a function that proces point forecasts for the model function.
#' @param rolling should a rolling procedure be used?
#' @param window llength of the window to build each model
#' @param horizon length of the forecast horizon to use for computing errors
#' 
cvts <- function(data, FUN, FCFUN, rolling = TRUE, window = 10, horizon = 5){
   actual <- data
   # this will be replaced with the fitted values
   forecasted <- data
   return(accuracy(f = forecasted, x = data))
}
