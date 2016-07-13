# Unit tests on the cvts function
if(require(forecast) &  require(testthat)){
  context("Testing input for cvts()")
  test_that("Testing invalid inputs", {
    expect_error(cvts("invalid"))
    expect_error(cvts(AirPassengers, useHorizon = 0L))
    expect_error(cvts(AirPassengers, windowSize = 3.2))
    expect_error(cvts(AirPassengers, windowSize = 130, maxHorizon = 12))
  })
  test_that("Testing valid inputs", {
     expect_error(cvts(AirPassengers, rolling = FALSE, windowSize = 60, maxHorizon = 12), NA)
  })
}
