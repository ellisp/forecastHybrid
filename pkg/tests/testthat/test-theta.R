# tests that the forecast(thetam(...)) returns the same results as thetaf(...)


# real seasonal data
fc1 <- thetaf(AirPassengers, h = 12)
fc2 <- forecast(thetam(AirPassengers), h = 12)

expect_equal(sum(fc2$mean - fc1$mean), 0)
expect_equal(sum(fc2$upper - fc1$upper), 0)
expect_equal(sum(fc2$lower - fc1$lower), 0)


# simulated seasonal data
x <- ts(rnorm(100) + c(0, 1, 2, 3), frequency = 4)
expect_equal(sum(forecast(thetam(x), h = 4)$mean - thetaf(x, h = 4)$mean ), 0)


# simulated non-seasonal data
x <- ts(rnorm(100))
expect_equal(sum(forecast(thetam(x), h = 4)$mean - thetaf(x, h = 4)$mean ), 0)


# fan
fan1 <- forecast(thetam(AirPassengers), fan = TRUE)
fan2 <- thetaf(AirPassengers, fan = TRUE)
expect_equal(sum(fan1$upper - fan2$upper), 0)
expect_equal(sum(fan1$lower - fan2$lower), 0)

# arbitrary level
fan1 <- forecast(thetam(AirPassengers), level = c(75, 90))
fan2 <- thetaf(AirPassengers, level = c(75, 90))
expect_equal(sum(fan1$upper - fan2$upper), 0)
expect_equal(sum(fan1$lower - fan2$lower), 0)

