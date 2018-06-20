
context('distance calculating')

test_that('distance returned must be less than 0.5% compared with geosphere::distHaversine', {
  temp <- distanceCaculate(0, 0, 1, 1)
  temp_std <- 157425.5 # geosphere::distHaversine(c(0, 0), c(1, 1))
  error <- abs(temp_std - temp) / temp_std
  expect_lte(error, 0.005)
})
