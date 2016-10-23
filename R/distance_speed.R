#' Mutate a track
#' Add derived variables: distance from previous trackpoint,
#' elapsed time since last trackpoint, speed (kph), bearing.
#'
#' @param track
#'
#' @return data.frame
#' @export
#'
#' @examples
trk_mutate <- function(track) {
  rows <- nrow(track) - 1
  track$lat_prev <- c(NA, head(track$lat, rows))
  track$lon_prev <- c(NA, head(track$lon, rows))
  track$dist <- haversine(track$lat, track$lon, track$lat_prev, track$lon_prev)
  track$elapsed = as.integer(track$time - head(track$time, 1))
  track$lastTime = c(NA, head(track$elapsed, rows))
  track$timeSeg = track$elapsed - track$lastTime
  track$kph = track$dist / track$timeSeg * 3.6
  track$bearing = bearing(track$lat, track$lon, track$lat_prev, track$lon_prev)
  track[, !names(track) %in% c('lat_prev', 'lon_prev', 'lastTime')]
}

# Extract coordinates from route or track

coord_first <- function(latlon)
  as.numeric(head(latlon, 1)[c('lat', 'lon')])

coord_last <- function(latlon)
  as.numeric(tail(latlon, 1)[c('lat', 'lon')])

coord_max <- function(latlon)
  c(max(latlon$lat), max(latlon$lon))

coord_min <- function(latlon)
  c(min(latlon$lat), min(latlon$lon))

coord_avg <- function(latlon)
  rowMeans(cbind(coord_max(latlon), coord_min(latlon)))

#' Distance between two points (haversine formula)
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return meters
#' @export
#'
#' @examples
haversine <- function(lat1, lon1, lat2, lon2) {
  rLat1 <- lat1 * (pi / 180)        # convert to radians
  rLon1 <- lon1 * (pi / 180)
  rLat2 <- lat2 * (pi / 180)
  rLon2 <- lon2 * (pi / 180)
  dlon <- rLon2 - rLon1
  dlat <- rLat2 - rLat1
  a <- (sin(dlat / 2))^2 + cos(rLat1) * cos(rLat2) * (sin(dlon / 2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378145
  R * c
}

#' Bearing
#'
#' @param lat1
#' @param lon1
#' @param lat2
#' @param lon2
#'
#' @return degrees
#' @export
#'
#' @examples
bearing <- function (lat1, lon1, lat2, lon2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  bear <- atan2(sin(dlon) * cos(b1), cos(a1) * sin(b1) - sin(a1) *
                  cos(b1) * cos(dlon))
  bear %% (2 * pi) * (180 / pi)
}

#' Distance from start, in a straight line
#'
#' @param track
#' @param route
#'
#' @return meters
#' @export
#'
#' @examples
dist_from_start <- function(track, route = NULL) {
  if (is.null(route))  {
    haversine(coord_first(track)[1], coord_first(track)[2],
              coord_last(track)[1],  coord_last(track)[2])
  } else {
    haversine(coord_first(route)[1], coord_first(route)[2],
              coord_last(track)[1],  coord_last(track)[2])
  }
}

#' Distance between current position and finish
#'
#' @param track
#' @param route
#'
#' @return meters
#' @export
#'
#' @examples
dist_to_finish <- function(track, route) {
  haversine(coord_last(track)[1], coord_last(track)[2],
            coord_last(route)[1],  coord_last(route)[2])
}

speed <- function(meters, seconds) meters / seconds

#' Total track distance
#' Sum of distances between trackpoints
#'
#' @param track
#' @param mut_track
#'
#' @return meters
#' @export
#'
#' @examples
trk_dist <- function(track = NULL, track_mut = NULL) {
  if (!is.null(track_mut)) {
    sum(track_mut$dist, na.rm = T)
  } else {
    rows <- nrow(track) - 1
    track$lat_prev <- c(NA, head(track$lat, rows))
    track$lon_prev <- c(NA, head(track$lon, rows))
    track$dist <- haversine(track$lat, track$lon, track$lat_prev, track$lon_prev)
    sum(track$dist, na.rm = TRUE)
  }
}

trk_dist2 <- function(route) {  # recursive
  total <- 0
  lat1 <- route[[1, 'lat']]
  lon1 <- route[[1, 'lon']]
  lat2 <- route[[2, 'lat']]
  lon2 <- route[[2, 'lon']]

  if (nrow(route) == 2)
    total + haversine(lat1, lon1, lat2, lon2)
  else
    total <- total + trk_dist2(tail(route, -1))

  total + haversine(lat1, lon1, lat2, lon2)
}

#' Estimated time (in seconds) remaining to finish
#'
#' @param track_mut
#' @param meters_to_fin
#'
#' @return seconds
#' @export
#'
#' @examples
time_remain <- function(track_mut, meters_to_fin) {
  recentSpeed <- mean(tail(track_mut$kph, 5)) # last 5 trackpoints
  (meters_to_fin / 1000) / (recentSpeed * 3600)
}

#' Estimated finish time
#'
#' @param track_mut
#' @param route
#'
#' @return POSIXct
#' @export
#'
#' @examples
finish_time <- function(track_mut, route) {
  last_time <- tail(track_mut$time, 1)
  last_time + time_remain(track_mut, dist_to_finish(track_mut, route))
}

pct_done_dist <- function(track, route) {
  # only works if path to finish is straight line
  route_tot <- trk_dist(route)
  to_finish <- dist_to_finish(track, route)
  round(100 * (route_tot - to_finish) / route_tot, 0)
}

pct_done_time <- function(track_mut, route) {  # returns numeric
  elapsed <- track_elapsed_sec(track_mut)
  ETR <- time_remain(track_mut, dist_to_finish(track, route))
  total <- elapsed + ETR
  elapsed / total * 100
}
