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

#' Downsample a track with evenly spaced times to a target number of rows
#'
#' @param target_n
#' @param trk_data
#'
#' @export
#'
#' @return
trk_downsample <- function(trk_data, target_n) {
  df <- trk_data
  n_rows <- nrow(df)

  # Calculate the number of rows to keep in the downsampled dataframe
  # Return the input dataframe if it already has fewer rows than the target
  if (nrow(df) <= target_n)
    return(df)
  else
    n_keep <- target_n

  # Calculate the time span covered by the input dataframe
  time_span <- difftime(max(df$time), min(df$time), units = "secs")

  # Calculate the time interval between each kept row
  time_interval <- time_span / (n_keep - 1)

  # Keep the rows closest to each time interval
  times <- seq(from = as.POSIXct(min(df$time)),
               to = as.POSIXct(max(df$time)),
               by = time_interval)

  idx <- sapply(times, function(t) which.min(abs(df$time - t)))
  df_downsampled <- df[idx, ]

  return(df_downsampled)
}
