#' Convert seconds to hours:minutes:seconds
#'
#' @param seconds
#'
#' @return Vector of c(hours, minutes, seconds)
#' @export
#'
#' @examples
secs_to_HMS <- function(seconds) {
  hrs <- seconds %/% 3600
  min <- (seconds - hrs * 3600) %/% 60
  sec <- seconds - (min * 60 + hrs * 3600)
  c(hrs, min, sec)
}

#' Print Hours:Minutes
#'
#' @param seconds
#'
#' @return String - e.g., "10H 5M"
#' @export
#'
#' @examples
p_sec_HM <- function(seconds) {
    HM <- secs_to_HMS(seconds)[1:2]
    paste0(HM[1], "H ", HM[2], "M")
}

#' Elapsed time of track
#'
#' @param track
#'
#' @return seconds (int)
#' @export
#'
#' @examples
track_elap_sec <- function(track) {
  as.integer(tail(track$time, 1)) - as.integer(head(track$time, 1))
}

#' Print elapsed time of track
#'
#' Displays in Hours:Minutes
#' @param track
#'
#' @return String - e.g., 12:20
#' @export
#'
#' @examples
p_track_elap_HM <- function(track) {
  paste0(
    secs_to_HMS(
      track_elap_sec(track))[1], ":",
    sprintf("%02.f",
      secs_to_HMS(track_elap_sec(track))[2]))
}

#' Elapsed time since last trackpoint
#'
#' @param track
#' @param units
#'
#' @return difftime
#' @export
#'
#' @examples
track_since_last <- function(track, units) {
  last <- tail(track$time, 1)
  difftime(Sys.time(), last, units = units)
}
