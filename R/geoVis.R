#' @title visualize status
#'
#' @description visualize user's longitude and latitude on map
#'
#' @param df dataframe about users' geo information, including id, account_id,
#' longitude, latitude and status field at least.
#' @param ids integer vector, indicate the users whose status will be change to
#' 1 and their cirlces on map will be blue.
#'
#' @details
#' see users' status information on the map, the status field had already been
#' settle by SQL store procedure on the remote server, if \code{ids} were
#' provided, the circle represent ids will also been labeled as \code{status == 1}.
#' blue circles indicate \code{status == 1}, and red ones indicate opposite.
attenceVis <- function(df, ids = NULL) {
  require(dplyr)
  require(leaflet)

  pal <- colorFactor(c("red", "blue"), domain = c(0, 1))

  attence_id <- unique(df$id[df$status == 1])
  df$status[df$id %in% attence_id] <- 1

  if (!is.null(ids)) {
    df$status[df$id %in% ids] <- 1
  }

  df %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    mutate(longitude = longitude - 0.011,
           latitude = latitude - 0.004) %>%
    leaflet() %>%
    # setView(lng = median(df$longitude, na.rm = TRUE),
    #         lat = median(df$latitude, na.rm = TRUE),
    #         zoom = 15) %>%
    addCircles(lng = ~longitude,
               lat = ~latitude,
               color = ~pal(status),
               popup = ~as.character(
                 paste(id, account_id, sep = '-->')
               )) %>%
    addTiles()
}
