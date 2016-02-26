# Unit tests on the hybridModel function
if(require(fpp) & require(forecast) & require(testthat)){
  context("Testing input for forecast.hybridModel()")
  test_that("Testing invalid inputs", {
    hModel <- hybridModel(woolyrnq)
    expect_that(forecast.hybridModel(object = 1), throws_error())
    expect_that(forecast.hybridModel(object = hModel, h = -1L), throws_error())
    expect_that(forecast.hybridModel(object = hModel, h = "a"), throws_error())
    expect_that(forecast.hybridModel(object = hModel, h = 3.2), throws_error())
    expect_that(forecast.hybridModel(object = hModel, h = 6, xreg = matrix(1:5, nrow = 5, ncol = 2)), gives_warning())
  })
}
