if (require(forecast) & require(testthat)) {
  test_that("Testing thiefModel()", {
    h <- 2
    set.seed(42)
    series <- ts(rnorm(16), f = 4)
    fc <- thiefModel(series, h = h)
    expect_true(class(fc) == "forecast")
    expect_true(length(fc$mean) == h)

    # Test with non-default combination and models
    h <- 5
    series <- AirPassengers
    fc <- thiefModel(y = series, models = "ef", h = h, comb = "ols")
    expect_true(class(fc) == "forecast")
    expect_true(length(fc$mean) == h)
    expect_true(all(fc$mean > 0))
  })
}
