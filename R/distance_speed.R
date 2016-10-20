track_mutate <- function(df) {
  df %>%
    rename(lat1 = lat, lon1 = lon) %>%
    mutate(lat2 = c(NA, head(lat1, nrow(df) - 1)),
        lon2 = c(NA, head(lon1, nrow(df) - 1)),
        dist = haversine(lat1, lon1, lat2, lon2),
        elapsed = as.integer(time - head(time, 1)),
        lastTime = c(NA, head(elapsed, nrow(df) - 1)),
        timeSeg = elapsed - lastTime,
        kph = dist / timeSeg * 3.6,
        bearing = bearing(lat1, lon1, lat2, lon2)) %>%
    select(-lat2, -lon2, -lastTime) %>%
    rename(lat = lat1, lon = lon1)
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
#' @return meters (int)
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
  R * c  # returns meters
}

# STRAIGHT-LINE DISTANCE & SPEED

dist_from_start <- function(track, route = NULL) {
  # returns meters
  if (is.null(route))  {
    haversine(coord_first(track)[1], coord_first(track)[2],
              coord_last(track)[1],  coord_last(track)[2])
  } else {
    haversine(coord_first(route)[1], coord_first(route)[2],
              coord_last(track)[1],  coord_last(track)[2])
  }
}

trk_mut_dist <- function(track_mut) {
  # returns meters
  sum(track_mut$dist, na.rm = T)
}

dist_to_finish <- function(track, route) {
  # returns meters
  haversine(coord_last(track)[1], coord_last(track)[2],
            coord_last(route)[1],  coord_last(route)[2])
}

speed <- function(meters, seconds) meters / seconds

trk_dist <- function(route) {   # 2 columns - lat, lon
  route <- route %>%
    select(lat1 = lat, lon1 = lon) %>%
    mutate(lat2 = c(NA, head(lat1, nrow(route) - 1))) %>%
    mutate(lon2 = c(NA, head(lon1, nrow(route) - 1))) %>%
    mutate(dist = haversine(lat1, lon1, lat2, lon2))

  sum(route$dist, na.rm=T)   # returns meters
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
    total <- total + route_dist2(tail(route, -1))

  total + haversine(lat1, lon1, lat2, lon2)
}

time_remain <- function(track_mut, meters_to_fin) {
  # returns numeric (seconds)
  recentSpeed <- mean(tail(track_mut$kph, 5)) # last 5 trackpoints
  conv_m_km(meters_to_fin) / recentSpeed * 3600
}

finish_time <- function(track_mut, route) {  # returns POSIXct
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

bearing <- function (lat1, lon1, lat2, lon2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- lon1 * rad
  b1 <- lat2 * rad
  b2 <- lon2 * rad
  dlon <- b2 - a2
  bear <- atan2(sin(dlon) * cos(b1), cos(a1) * sin(b1) - sin(a1) *
                  cos(b1) * cos(dlon))
  bear %% (2 * pi) * (180 / pi)  # returns degrees
}
