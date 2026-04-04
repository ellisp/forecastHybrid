
test_that("Testing thiefModel()", {
  h <- 2L
  set.seed(42L)
  series <- ts(rnorm(16L), f = 4L)
  fc <- thiefModel(series, h = h)
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, h)

  # Test with non-default combination and models
  h <- 5L
  series <- AirPassengers
  fc <- thiefModel(y = series, models = "ef", h = h, comb = "ols")
  expect_s3_class(fc, "forecast")
  expect_length(fc$mean, h)
  expect_true(all(fc$mean > 0L))
})
