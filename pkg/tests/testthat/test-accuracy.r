test_that("accuracy generic function works", {
   hm <- hybridModel(wineind)
   expect_error(accuracy(hm), NA)
   expect_error(accuracy(hm$ets), NA)
   expect_error(accuracy(hm$auto.arima), NA)
   expect_error(accuracy(hm$nnetar), NA)
   expect_error(accuracy(hm$stlm), NA)
   expect_error(accuracy(hm$tbats), NA)
   expect_error(accuracy(hm$thetam), NA)
   
})