#' @title caculate distance
#'
#' @description caculate the spherical distance between 2 point on earth
#'
#' @param lon_1,lon_2 numeric, degree longitude
#' @param lat_1,lat_2 numeric, degree latitude
#' @param r earth radius from \code{geosphere} package
#'
#' @details
#' although \code{geosphere::distHaversine} can caculate the spherical distance,
#' \code{distanceCaculate} is about 20X-30X faster than that,
#' and only has error less than 0.2\%.
#'
#' @examples
#' distanceCaculate(108.8821, 34.2341, 108.882, 34.2343)
#' @export
#'
distanceCaculate <- function(lon_1, lat_1, lon_2, lat_2, r = 6378137) {
  dx <- lon_1 - lon_2
  dy <- lat_1 - lat_2

  b <- (lat_1 + lat_2) / 2

  lx <- (0.9971 + 0.0004533 * b + -0.0001759 * b^2 + 0.0000005029 * b^3) * dx

  sqrt(lx ^ 2 + dy ^ 2) * pi * r / 180
}
