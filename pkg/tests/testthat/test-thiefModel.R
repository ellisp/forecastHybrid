if(require(forecast) & require(testthat)){
  test_that("Testing thiefModel()", {
    h <- 5
    fc <- thiefModel(AirPassengers, models = "aefnt", h = 5)
    expect_true(class(fc) == "forecast")
    expect_true(length(fc$mean) == 5)
  })
}
