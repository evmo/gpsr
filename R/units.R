MILES_TO_KM     <- 1.6093445
KM_TO_MILES     <- 1 / MILES_TO_KM
KM_TO_METERS    <- 1000
METERS_TO_KM    <- 1 / KM_TO_METERS
METERS_TO_MILES <- 1 / (KM_TO_METERS * MILES_TO_KM)

conv_unit <- function(from, multiplier) {
  from * multiplier
}