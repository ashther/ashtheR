
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