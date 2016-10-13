# TIME --------------------------

conv_sec_HMS <- function(seconds) {
  hrs <- seconds %/% 3600
  min <- (seconds - hrs * 3600) %/% 60
  sec <- seconds - (min * 60 + hrs * 3600)
  c(hrs, min, sec)
}

p_conv_sec_HMS <- function(seconds) {
    HM <- conv_sec_HMS(seconds)[1:2]
    paste0(HM[1], "H ", HM[2], "M")
}

track_elap_sec <- function(track) {
  as.integer(tail(track$time, 1)) - as.integer(head(track$time, 1))
}

p_track_elap_HM <- function(track) {
  paste0(
    conv_sec_HMS(
      track_elap_sec(track))[1], ":",
    sprintf("%02.f", 
      conv_sec_HMS(track_elap_sec(track))[2]))
}

track_since_last <- function(track, units) {
  last <- tail(track$time, 1)
  difftime(Sys.time(), last, units = units)
}


sinceLast <- function(track) {
  lastTime <- strftime(tail(track$time, 1), format = "%a %d %b %H:%M")
  ago <- track_since_last(track, "mins")
  if (ago < 120) 
    paste0(lastTime, " (", round(ago), " min ago)")
  else 
    lastTime
}

# Extract coordinates from route or track

coord_first <- function(latlon)  as.numeric(head(latlon, 1)[c('lat', 'lon')])
coord_last <- function(latlon)   as.numeric(tail(latlon, 1)[c('lat', 'lon')])
coord_max <- function(latlon)    c(max(latlon$lat), max(latlon$lon))
coord_min <- function(latlon)    c(min(latlon$lat), min(latlon$lon))
coord_avg <- function(latlon) {
  rowMeans(cbind(coord_max(latlon), coord_min(latlon)))
}

# DISTANCE ----------------------------------------

conv_km_mi <- function(km)     km * 0.621371
conv_m_km <- function(meters)  meters / 1000
conv_m_mi <- function(meters)  conv_m_km(meters) %>% conv_km_mi

conv_dist <- function(distance, units) {
  if (units == "U.S.") 
    conv_m_mi(distance)
  else 
    conv_m_km(distance)
}

fmt_dist <- function(distance, units) {
  if (units == "U.S.")
    distance %>% round(1) %>% paste('mi')
  else
    distance %>% round(1) %>% paste('km')
}

fmt_speed <- function(speed, units) {
  if (units == "U.S.")
    speed %>% round(1) %>% paste('mph')
  else
    speed %>% round(1) %>% paste('kph')
}

print_dist <- function(distance, units) {
  conv_dist(distance, units) %>% fmt_dist(units)
}

print_speed <- function(speed, units) {
  conv_dist(speed, units) %>% fmt_speed(units)
}

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

dist_gps <- function(track_mut) {
  # returns meters
  sum(track_mut$dist, na.rm = T)
}

dist_to_finish <- function(track, route) {
  haversine(coord_last(track)[1], coord_last(track)[2], 
            coord_last(route)[1],  coord_last(route)[2])
}

speed <- function(meters, seconds) meters / seconds

route_dist <- function(route) {   # 2 columns - lat, lon
  route <- route %>%
    select(lat1 = lat, lon1 = lon) %>%
    mutate(lat2 = c(NA, head(lat1, nrow(route) - 1))) %>%
    mutate(lon2 = c(NA, head(lon1, nrow(route) - 1))) %>%
    mutate(dist = haversine(lat1, lon1, lat2, lon2))

  sum(route$dist, na.rm=T)   # returns meters
}

route_dist2 <- function(route) {  # recursive
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
  route_tot <- route_dist(route)
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

