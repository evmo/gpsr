# route2map.R
# input: route definition file (csv)
#  cols: num, lat, lon, desc
# output: html leaflet file

mapPath <-
  function(map, data, labelColumn = NULL, circleColor = "#FF4900",
           lineColor = "#FF4900", noHide = F) {
    map <- map %>%
      addCircles(data$lon, data$lat, color = circleColor, radius = 50) %>%
      addMarkers(
        data$lon,
        data$lat,
        label = data[, labelColumn],
        labelOptions = labelOptions(offset = c(15,-15), noHide = noHide),
        options = markerOptions(opacity = 0)
      ) %>%
      addPolylines(
        data$lon,
        data$lat,
        color = lineColor,
        weight = 3,
        dashArray = '5,5'
      )
    return(map)
  }

map_gps <- function(track = NULL,
                    route = NULL,
                    save = FALSE) {
  map <- leaflet() %>%
    addProviderTiles("Esri.NatGeoWorldMap",
                     group = "Standard",
                     options = providerTileOptions(attribution = "")
    ) %>%
    addProviderTiles("Esri.WorldImagery",
                     group = "Satellite",
                     options = providerTileOptions(attribution = "")
    ) %>%
    addLayersControl(baseGroups = c("Standard", "Satellite"))

  if (missing(track) && missing(route))
    return(map)

  if (!is.null(track))
    map <- map %>% mapPath(data = track, labelColumn = "time")

  if (!is.null(route))
    map <-
      map %>% mapPath(
        data = route,
        labelColumn = "desc",
        lineColor = "black",
        noHide = T
      )

  if (save == T)
    saveWidget(map, file = "~/Downloads/map.html", selfcontained = T)

  return(map)
}

# label = format(track$time + 3600 * hours_from_GMT, "%H:%M")
