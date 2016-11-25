distanceCaculate <- function(lon_1, lat_1, lon_2, lat_2) {
  dx <- lon_1 - lon_2
  dy <- lat_1 - lat_2
  
  b <- (lat_1 + lat_2) / 2
  
  lx <- (0.9971 + 0.0004533 * b + -0.0001759 * b^2 + 0.0000005029 * b^3) * dx
  
  sqrt(lx ^ 2 + dy ^ 2) * 111125
}