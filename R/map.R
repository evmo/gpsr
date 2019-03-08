#' Create a blank map
#'
#' @param providers character vector of tile providers
#' @param labels character vector of provider labels
#'
#' @return map
#' @import leaflet
#' @importFrom purrr walk2
#' @importFrom magrittr '%<>%'
#' @export
#'
#' @examples
base_map <- function(providers = NULL, labels = NULL) {
  map <- leaflet()

  if (is.null(providers))
    map %<>% addTiles
  else if (length(providers) == 1) {
    map %<>%
      addProviderTiles(
        providers,
        options = providerTileOptions(attribution = "")
      )
  } else {
    walk2(providers, labels, function(p, l) {
      map <<- map %>%
        addProviderTiles(p, group = l, options =
                           providerTileOptions(attribution = ""))
    })
    map %<>% addLayersControl(baseGroups = labels)
  }

  return(map)
}

#' Map Path Points
#'
#' @param map
#' @param data
#' @param legendGroup
#' @param circleColor
#' @param circleRadius
#' @param ...
#'
#' @return map
#' @import leaflet
#' @export
#'
#' @examples
map_path_points <- function(map, data, legendGroup = NULL, circleColor = "#FF4900",
                            circleRadius = 50, ...) {
  map %>% addCircles(
    data$lon,
    data$lat,
    group = legendGroup,
    color = circleColor,
    radius = circleRadius,
    ...
  )
}

#' Map Path Lines
#'
#' @param map
#' @param data
#' @param legendGroup
#' @param lineColor
#' @param ...
#'
#' @return map
#' @import leaflet
#' @export
#'
#' @examples
map_path_lines <- function(map, data, legendGroup = NULL,
                           lineColor = "#FF4900", ...) {
  map %>% addPolylines(
    data$lon,
    data$lat,
    group = legendGroup,
    color = lineColor,
    weight = 3,
    ...
  )
}

add_map_labels <- function(map, data, labels, legendGroup = NULL, ...) {
  map %>% addMarkers(
    data$lon,
    data$lat,
    label = labels,
    group = legendGroup,
    labelOptions = labelOptions(offset = c(15, -15), ...),
    options = markerOptions(opacity = 0)
  )
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
                 circleColor = 'black',
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

map_traccar <- function(deviceid, start_time, stop_time,
                        db, host, port, user, password) {
  d <- read_traccar(deviceid, start_time, stop_time,
                    db, host, port, user, password)
  map_gps(d)
}
