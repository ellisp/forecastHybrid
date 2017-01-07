
test_that("forecast nnetar prediction intervals with different levels work", {
   inputSeries <- ts(rnorm(9), f = 4)
   hm2 <- hybridModel(inputSeries, models = "afn", weights = "equal")
   # forecast with nnetar and PI = TRUE is very slow, so use few npaths
   expect_error(forecast(hm2, h = 48, npaths = 10), NA)
   expect_error(forecast(hm2, h = 48, level = c(70, 80, 90, 95), npaths = 10), NA)
})
