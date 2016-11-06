
test_that("forecast nnetar prediction intervals with different levels work", {
   hm2 <- hybridModel(wineind, models = "anst", weights = "equal")
   expect_error(forecast(hm2, h = 48), NA)
   expect_error(forecast(hm2, h = 48, level = c(70, 80, 90, 95)), NA)
})
