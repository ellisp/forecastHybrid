Sys.setenv("R_TESTS" = "")
if(require(testthat) & require(fpp) & require(forecast)){
  test_check("forecastHybrid")
  }
