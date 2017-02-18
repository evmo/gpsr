#' Create a blank map
#'
#' @return leaflet map
#' @export
#'
#' @examples
base_map <- function() {
  leaflet() %>%
    addProviderTiles("Esri.NatGeoWorldMap",
                     group = "Standard",
                     options = providerTileOptions(attribution = "")) %>%
    addProviderTiles("Esri.WorldImagery",
                     group = "Satellite",
                     options = providerTileOptions(attribution = "")) %>%
    addLayersControl(baseGroups = c("Standard", "Satellite"))
}

#' Display series of coordinates on a map
#'
#' @param map
#' @param data
#' @param labelColumn
#' @param circleColor
#' @param lineColor
#' @param noHide
#'
#' @return a leaflet map
#' @importFrom leaflet addCircles addMarkers addPolylines
#' @export
#'
#' @examples
map_path <-
  function(map,
           data,
           labels = NULL,
           circleColor = "#FF4900",
           circleRadius = 50,
           circleOpacity = NULL,
           circleFillOpacity = NULL,
           connectPoints = TRUE,
           lineColor = "#FF4900",
           lineType = NULL,
           noHide = F,
           legendGroup = NULL) {

    map <- map %>%
      addCircles(
        data$lon,
        data$lat,
        group = legendGroup,
        color = circleColor,
        radius = circleRadius,
        opacity = circleOpacity,
        fillOpacity = circleFillOpacity)

    if (connectPoints == TRUE) {
      map <- map %>%
        addPolylines(
          data$lon,
          data$lat,
          group = legendGroup,
          color = lineColor,
          weight = 3,
          dashArray = lineType
        )
    }

    if (!is.null(labels)) {
      map <- map %>%
        addMarkers(
          data$lon,
          data$lat,
          label = labels,
          group = legendGroup,
          labelOptions = labelOptions(offset = c(15, -15), noHide = noHide),
          options = markerOptions(opacity = 0)
        )
    }

    return(map)
  }

#' Display a GPS track and/or route on a map
#'
#' @param track
#' @param route
#' @param save
#'
#' @return leaflet map
#' @importFrom leaflet leaflet addProviderTiles addLayersControl
#' providerTileOptions markerOptions labelOptions
#' @importFrom htmlwidgets saveWidget
#' @export
#'
#' @examples
map_gps <- function(track = NULL,
                    route = NULL,
                    track_labels = NULL,
                    save = FALSE) {
  map <- base_map()

  if (missing(track) && missing(route))
    return(map)

  if (!is.null(track)) {
    map <- map %>%
      map_path(data = track, labels = track_labels, connectPoints = FALSE)
    if (!is.null(route)) {
      map <- map %>%
        map_path(data = route,
                 lineColor = 'white',
                 circleOpacity = 0,
                 circleFillOpacity = 0,
                 noHide = T)
    }
  } else {  # route only
    map <-
      map %>% map_path(
        data = route,
        lineColor = "white",
        noHide = T
      )
  }

  if (save == T)
    saveWidget(map, file = "~/Downloads/map.html")

  return(map)
}

# label = format(track$time + 3600 * hours_from_GMT, "%H:%M")
