#' Create a blank map
#'
#' @param providers character vector of tile providers
#' @param labels character vector of provider labels
#'
#' @return map
#' @import leaflet
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
    purrr::walk2(providers, labels, function(p, l) {
      map <<- map %>%
        addProviderTiles(p, group = l, options =
                           providerTileOptions(attribution = ""))
    })
    map %<>% addLayersControl(baseGroups = labels)
  }

  return(map)
}

#' Add path points to map
#'
#' @param map leaflet map
#' @param data track data
#' @param legendGroup
#' @param circleColor
#' @param circleRadius
#' @param ...
#'
#' @return map
#' @export
#'
#' @examples
map_path_points <- function(map, data, legendGroup = NULL, circleColor = "#FF4900",
                            circleRadius = 50, ...) {
  map %>% leaflet::addCircles(
    data$lon,
    data$lat,
    group = legendGroup,
    color = circleColor,
    radius = circleRadius,
    ...
  )
}

#' Add path lines to map
#'
#' @param map leaflet map
#' @param data track data
#' @param legendGroup
#' @param lineColor
#' @param ...
#'
#' @return map
#' @export
#'
#' @examples
map_path_lines <- function(map, data, legendGroup = NULL,
                           lineColor = "#FF4900", ...) {
  map %>% leaflet::addPolylines(
    data$lon,
    data$lat,
    group = legendGroup,
    color = lineColor,
    weight = 3,
    ...
  )
}

#' Add labels to points on map
#'
#' @param map leaflet map
#' @param data track data
#' @param labels
#' @param legendGroup
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
add_map_labels <- function(map, data, labels, legendGroup = NULL, ...) {
  map %>% leaflet::addMarkers(
    data$lon,
    data$lat,
    label = labels,
    group = legendGroup,
    labelOptions = labelOptions(offset = c(15, -15), ...),
    options = markerOptions(opacity = 0)
  )
}

#' Create a map of a track
#'
#' @param track track data
#' @param freq frequency for trk_reduce
#' @param tz_offset hours offset from GMT
#' @param providers
#' @param labels
#' @param circleColor
#' @param lineColor
#'
#' @return
#' @export
#'
#' @examples
map_track <- function(track, freq = "60 min", tz_offset = 0, providers = NULL,
                      labels = NULL, circleColor = NULL, lineColor = NULL) {
  t = trk_reduce(track, freq)
  base_map(providers, labels) %>%
    map_path_points(data = t, circleColor) %>%
    map_path_lines(data = t, lineColor) %>%
    add_map_labels(
      data = t,
      labels = format(t$time + tz_offset * 3600, "%H:%M")
    )
}

# map_traccar <- function(deviceid, start_time, stop_time,
#                         db, host, port, user, password) {
#   d <- read_traccar(deviceid, start_time, stop_time,
#                     db, host, port, user, password)
#   map_gps(d)
# }
