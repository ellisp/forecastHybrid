# Unit tests on the generic function
context("Testing helper functions")
test_that("Testing internal helper functions", {
  # Test getModelName()
  expect_identical(getModelName("a"), "auto.arima")
  expect_identical(getModelName("e"), "ets")
  expect_identical(getModelName("f"), "thetam")
  expect_identical(getModelName("n"), "nnetar")
  expect_identical(getModelName("s"), "stlm")
  expect_identical(getModelName("t"), "tbats")
  expect_identical(getModelName("x"), "arfima")
  expect_identical(getModelName("z"), "snaive")

  # Test getModel()
  expect_true(is.function(getModel("a"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("e"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("f"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("n"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("s"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("t"))) # nolint: expect_type_linter
  expect_true(is.function(getModel("z"))) # nolint: expect_type_linter
  expect_identical(getModel("a"), auto.arima)
  expect_identical(getModel("e"), ets)
  expect_identical(getModel("f"), thetam)
  expect_identical(getModel("n"), nnetar)
  expect_identical(getModel("s"), stlm)
  expect_identical(getModel("t"), tbats)
  expect_identical(getModel("x"), arfima)
  expect_identical(getModel("z"), snaive)

  # Test unwrapParallelModels
  models <- list(thetam(wineind), stlm(wineind))
  modelNames <- c("f", "s")
  results <- unwrapParallelModels(models, modelNames)
  expect_length(results, 2)
  expect_true(all(names(results) == c("thetam", "stlm")))
})

test_that("Testing tsSubsetWithIndices()", {
  expect_s3_class(tsSubsetWithIndices(wineind, 3:17), "ts")
  expect_identical(head(wineind, 4), tsSubsetWithIndices(wineind, 1:4))
  expect_identical(head(wineind, 5), tsSubsetWithIndices(wineind, 1:5))

  # Invalid subset outside of range
  expect_error(tsSubsetWithIndices(wineind, length(wineind) + 1))

  # Non-continuous indices
  expect_error(tsSubsetWithIndices(wineind, 4:1))
  expect_error(tsSubsetWithIndices(wineind, c(3, 7)))
})
test_that("Testing tsCombine", {
  ts1 <- tsSubsetWithIndices(AirPassengers, 1:50)
  ts2 <- tsSubsetWithIndices(AirPassengers, 50:100)
  ts3 <- tsSubsetWithIndices(AirPassengers, 101:144)
  combinedTs <- tsCombine(ts1, ts2, ts3)
  expect_s3_class(combinedTs, "ts")
  expect_length(combinedTs, length(AirPassengers))
  expect_equal(combinedTs, AirPassengers)

  # Test that a recursive combine works as well
  rCombinedTs <- tsCombine(ts1, tsCombine(ts2, ts3))
  expect_s3_class(rCombinedTs, "ts")
  expect_length(rCombinedTs, length(AirPassengers))
  expect_equal(rCombinedTs, AirPassengers)
})
