#' Create a blank map
#'
#' @param provider_tiles character vector of known map tile providers
#' @param custom_tiles character vector of URLs of custom tiles
#' @param p_tile_opts list of providerTileOptions for the provider tiles
#' @param c_tile_opts list of tileOptions for the custom tiles
#' @param p_tile_labs character vector of labels for the provider tiles
#' @param c_tile_labs character vector of labels for the custom tiles
#' @param proxy_map_id map ID of a leaflet map if called from a Shiny app
#'
#' @return map a leaflet map
#' @import leaflet
#' @importFrom magrittr '%<>%'
#' @export
#'
#' @examples
base_map <- function(provider_tiles = NULL, custom_tiles = NULL,
   p_tile_opts = list(), c_tile_opts = list(),
   p_tile_labs = NULL, c_tile_labs = NULL,
   proxy_map_id = NULL) {

  # validate args
  if (!missing(p_tile_opts))
    stopifnot(length(p_tile_opts) == length(provider_tiles))
  if (!missing(p_tile_labs))
    stopifnot(length(p_tile_labs) == length(provider_tiles))
  if (!missing(c_tile_opts))
    stopifnot(length(c_tile_opts) == length(custom_tiles))
  if (!missing(c_tile_labs))
    stopifnot(length(c_tile_labs) == length(custom_tiles))

  # are we modifying an existing map in a Shiny app?
  if (missing(proxy_map_id))
    map <- leaflet()
  else
    map <- leafletProxy(proxyMapId = proxy_map_id)

  n_p_tiles <- length(provider_tiles)
  n_c_tiles <- length(custom_tiles)
  n_tilesets <- n_p_tiles + n_c_tiles

  # no tiles provided by user
  if (n_tilesets == 0)
    map %<>% addTiles
  # one tileset
  else if (n_tilesets == 1) {
    # leaflet provider
    if (n_p_tiles == 1) {
      map %<>%
        addProviderTiles(
          provider = provider_tiles,
          options = providerTileOptions(p_tile_opts)
        )
    } else {
      # custom tileset
      map %<>%
        addTiles(
          urlTemplate = custom_tiles,
          options = tileOptions(c_tile_opts)
        )
    }
  # multiple providers
  } else {
    # tileset labels default to name of tileset
    if (missing(p_tile_labs) && missing(c_tile_labs)) {
      p_tile_labs <- provider_tiles
      c_tile_labs <- custom_tiles
    }

    # if no options provided, provide empty list
    if (length(p_tile_opts) == 0)
      pto <- rep(list(list()), length(provider_tiles))
    else
      pto <- p_tile_opts
    if (length(c_tile_opts) == 0)
      cto <- rep(list(list()), length(custom_tiles))
    else
      cto <- c_tile_opts

    # iteratively add each tileset to the map
    if (n_p_tiles >= 1) {
      purrr::pwalk(list(provider_tiles, p_tile_labs, pto),
        function(tiles, labs, opts) {
          map <<- map %>%
            addProviderTiles(
              provider = tiles,
              group = labs,
              options = do.call(providerTileOptions, opts)
            )
        }
      )
    }

    if (n_c_tiles >= 1) {
      purrr::pwalk(list(custom_tiles, c_tile_labs, cto),
        function(tiles, labs, opts) {
          map <<- map %>%
            addTiles(
              urlTemplate = tiles,
              group = labs,
              options = do.call(tileOptions, opts)
            )
        }
      )
    }

    # add the tile selector
    map %<>% addLayersControl(baseGroups = c(p_tile_labs, c_tile_labs))
  }  #  end if/else

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
                            circleRadius = 5, ...) {
  map %>% leaflet::addCircleMarkers(
    data$lon,
    data$lat,
    group = legendGroup,
    color = circleColor,
    radius = circleRadius,
    fillOpacity = 0.7,
    stroke = FALSE,
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
    weight = 2,
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
#' @param provider_tiles character vector of known map tile providers
#' @param custom_tiles character vector of URLs of custom tiles
#' @param p_tile_opts list of providerTileOptions for the provider tiles
#' @param c_tile_opts list of tileOptions for the custom tiles
#' @param p_tile_labs character vector of labels for the provider tiles
#' @param c_tile_labs character vector of labels for the custom tiles
#' @param labels labels for each trackpoint
#' @param circleColor color of each trackpoint
#' @param lineColor color line connecting the trackpoints
#'
#' @return
#' @export
#'
#' @examples
trk_map <- function(track, freq = "60 min", tz_offset = 0,
  provider_tiles = NULL, custom_tiles = NULL,
  p_tile_opts = list(), c_tile_opts = list(),
  p_tile_labs = NULL, c_tile_labs = NULL,
  labels = NULL, circleColor = NULL, lineColor = NULL) {

  t = trk_reduce(track, freq)
  base_map(provider_tiles, custom_tiles, p_tile_opts, c_tile_opts,
           p_tile_labs, c_tile_labs) %>%
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

