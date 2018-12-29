if(require(forecast) & require(testthat)){
  test_that("Accuracy generic function works", {
    inputSeries <- ts(rnorm(9), f = 4)
    hm <- hybridModel(inputSeries)
    expect_error(accuracy(hm), NA)
    expect_error(accuracy(hm$ets), NA)
    expect_error(accuracy(hm$auto.arima), NA)
    expect_error(accuracy(hm$nnetar), NA)
    expect_error(accuracy(hm$stlm), NA)
    expect_error(accuracy(hm$tbats), NA)
    expect_error(accuracy(hm$thetam), NA)
  })
}
