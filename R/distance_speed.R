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

#' Calculate route distance
#'
#' @param route df with 2 cols, lat and lon
#'
#' @return
#' @export
#'
#' @examples
route_dist <- function(route) {
  rows <- nrow(route) - 1
  route$lat_prev <- c(NA, head(route$lat, rows))
  route$lon_prev <- c(NA, head(route$lon, rows))
  route$seg_dist <- haversine(route$lat, route$lon, route$lat_prev, route$lon_prev)
  sum(route$seg_dist, na.rm = T)
}

#' Mutate a track (dplyr implementation)
#' Add derived variables: distance from previous trackpoint,
#' elapsed time since last trackpoint, speed (kph), bearing.
#'
#' @param track a data.frame with 3 cols: 'lat', 'lon', 'time'
#'
#' @return a data.frame with 4 additional cols
#' @importFrom dplyr mutate
#'
#' @examples
trk_mutate_dplyr <- function(track) {
  first_time <- track[1, "time"] %>% pull
  mutate(track,
    lat_prev = lag(lat, 1),
    lon_prev = lag(lon, 1),
    seg_dist = haversine(lat, lon, lat_prev, lon_prev),
    elapsed = as.integer(time - first_time),
    elapsed_prev = lag(elapsed, 1),
    seg_elapsed = elapsed - elapsed_prev,
    kph = seg_dist / seg_elapsed * 3.6
  ) %>%
    select(-ends_with("_prev"))
}

#' Mutate a track (base R implementation)
#' Add derived variables: distance from previous trackpoint,
#' elapsed time since last trackpoint, speed (kph), bearing.
#'
#' @param track a data.frame with 3 cols: 'lat', 'lon', 'time'
#'
#' @return data.frame
#' @export
#'
#' @examples
trk_mutate <- function(track) {
  rows <- nrow(track) - 1
  track$lat_prev <- c(NA, head(track$lat, rows))
  track$lon_prev <- c(NA, head(track$lon, rows))
  track$seg_dist <- haversine(track$lat, track$lon, track$lat_prev, track$lon_prev)
  track$elapsed = as.integer(track$time - head(track$time, 1))
  track$elapsed_prev = c(NA, head(track$elapsed, rows))
  track$seg_elapsed = track$elapsed - track$elapsed_prev
  track$kph = track$seg_dist / track$seg_elapsed * 3.6
  track$bearing = bearing(track$lat, track$lon, track$lat_prev, track$lon_prev)
  track[, names(track)[!endsWith(names(track), "_prev")]]
}

head1 <- function(d) head(d, 1)
tail1 <- function(d) tail(d, 1)

#' Extract coordinates from route or track
#'
#' @param track data.frame
#' @param func function, e.g., min, max, mean
#'
#' @return c(lat, lon)
#' @export
#'
#' @examples
extract_coord <- function(track, func) {
  FUN = match.fun(func)
  c(FUN(track$lat), FUN(track$lon))
}

#' Distance from first to last trackpoint (straight line)
#'
#' @param track
#'
#' @return dbl meters
#' @export
#'
#' @examples
dist_from_start <- function(track) {
  start <- extract_coord(track, head1)
  current <- extract_coord(track, tail1)
  haversine(start[1], start[2], current[1], current[2])
}

#' Distance between current position and finish
#'
#' @param track
#' @param route
#'
#' @return dbl meters
#' @export
#'
#' @examples
dist_to_finish <- function(track, route) {
  current <- extract_coord(track, tail1)
  finish <- extract_coord(route, tail1)
  haversine(start[1], start[2], current[1], current[2])
}

#' Total track distance (sum of trackpoints)
#'
#' @param mut_track mutated track (via trk_mutate)
#'
#' @return meters
#' @export
#'
#' @examples
trk_dist <- function(track_mut) {
  sum(track_mut$seg_dist, na.rm = T)
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

#' Estimated time remaining to finish
#'
#' @param track_mut via trk_mutate()
#' @param meters_remain to finish
#'
#' @return dbl seconds
#' @export
#'
#' @examples
time_remain <- function(track_mut, meters_remain) {
  recentSpeed <- mean(tail(track_mut$kph, 5)) # last 5 trackpoints
  (meters_to_fin / 1000) / (recentSpeed * 3600)
}

#' Estimated time of arrival
#'
#' @param track_mut
#' @param route
#'
#' @return POSIXct arrival time
#' @export
#'
#' @examples
ETA <- function(track_mut, route) {
  last_time <- tail1(track_mut$time)
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
