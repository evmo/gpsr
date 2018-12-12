densifyRoute <- function(route, seg_dist = 100) {  # meters
  if (nrow(route) > 2) {
    densify <- data.frame(geosphere::makeLine(route[, c("lon", "lat")], seg_dist))
    densify <- densify[, c("lat", "lon")]
  } else {
    rdist <- route_dist(route)
    densify <- data.frame(geosphere::gcIntermediate(
      route[1, c("lon", "lat")],
      route[2, c("lon", "lat")],
      rdist / seg_dist))
    densify <- densify[, c("lat", "lon")]
  }
  rows <- nrow(densify) - 1
  densify$lat_prev <- c(NA, head(densify$lat, rows))
  densify$lon_prev <- c(NA, head(densify$lon, rows))
  densify$dist <- haversine(densify$lat, densify$lon,
                            densify$lat_prev, densify$lon_prev)
  total <- c(0, cumsum(densify[2:nrow(densify), 'dist']))
  cbind(densify, total)
}

# Progress along a non-straight-line route
# One-way, point-to-point
progressAlong <- function(track, routeDens) {
  currLoc <- track[nrow(track), c('lat', 'lon')]
  routeDist <- routeDens[nrow(routeDens), 'total']

  # Compute distance-from-current-location
  # of each point along densified route.
  routeDens$distFromCurr <- haversine(currLoc$lat, currLoc$lon,
                                      routeDens$lat, routeDens$lon)
  # Find point along densified route that
  # current location is closest to.
  closest <- routeDens[routeDens$distFromCurr == min(routeDens$distFromCurr),
                       c('lat', 'lon', 'total')]

  # how far along the route line is that point?
  closest$total / routeDist
}
