#' Seconds to c(H, M, S)
#'
#' @param seconds
#'
#' @return vector c(H, M, S)
#' @export
#'
#' @examples
sec2hms_v <- function(seconds) {
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
    HM <- sec2hms_v(seconds)[1:2]
    sprintf("%sH %sM", HM[1], HM[2])
}

#' Elapsed time of track
#'
#' @param track
#'
#' @return seconds (int)
#' @export
#'
#' @examples
trk_elapsed_sec <- function(track) {
  as.integer(tail(track$time, 1)) - as.integer(head(track$time, 1))
}

#' Print elapsed time of track ([H]H:MM)
#'
#' @param track
#'
#' @return String - e.g., 12:20
#' @export
#'
#' @examples
p_trk_elapsed_HM <- function(track) {
  hours <- sec2hms_v(trk_elapsed_sec(track))[1]
  minutes <- sec2hms_v(trk_elapsed_sec(track))[2]
  sprintf("%s:%02.f", hours, minutes)
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
trk_since_last <- function(track, units) {
  last <- tail(track$time, 1)
  difftime(Sys.time(), last, units = units)
}

#' Reduce trackpoint frequency
#'
#' @param track
#' @param interval
#'
#' @return
#' @export
#'
#' @examples
trk_reduce <- function(track, interval) {
  track$bin <- cut(track$time, breaks = interval)
  track[!duplicated(track$bin), c("lat","lon","time")]
}
