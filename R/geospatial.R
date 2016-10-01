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

route_dist <- function(route) {
  total <- 0
  lat1 <- route[[1, 'lat']]
  lon1 <- route[[1, 'lon']]
  lat2 <- route[[2, 'lat']]
  lon2 <- route[[2, 'lon']]

  if (nrow(route) == 2)
    total + haversine(lat1, lon1, lat2, lon2)
  else
    total <- total + route_dist(tail(route, -1))

  total + haversine(lat1, lon1, lat2, lon2)
}
