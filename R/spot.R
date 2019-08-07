#' Construct SPOT API request URL
#'
#' @param id
#' @param start
#' @param password
#'
#' @return
#' @export
#'
#' @examples
spot_url <- function(id, start = 0, password = NULL) {
  url <- glue::glue("https://api.findmespot.com/spot-main-web/\\
    consumer/rest-api/2.0/public/feed/{id}/message.json?start={start}")
  if (!is.null(password))
    url <- paste0(url, '&feedPassword=', password)
  return(url)
}

#' Convert raw SPOT messages to lat/lon/time data-frame
#'
#' @param msgs
#'
#' @return
#' @export
#'
#' @examples
spot_cols <- function(msgs) {
  msgs$offset <- as.integer(
    substr(msgs$dateTime,
           nchar(msgs$dateTime) - 4,
           nchar(msgs$dateTime) - 2)
  )
  msgs$time <- as.POSIXct(
    msgs$dateTime, tz = 'GMT', format = '%Y-%m-%dT%H:%M:%S%z'
  ) + 3600 * msgs$offset
  d <- msgs[, c("latitude", "longitude", "time")]
  names(d) <- c("lat", "lon", "time")
  return(d)
}

#' Convert raw SPOT messages to lat/lon/time data-frame (DT implementation)
#'
#' @param msgs
#'
#' @return
#' @import data.table magrittr
#' @export
#'
#' @examples
spot_colsDT <- function(msgs) {
  setDT(msgs) %>%
    .[, offset := as.integer(
      substr(dateTime, nchar(dateTime) - 4, nchar(dateTime) - 2)
      )] %>%
    .[, time := as.POSIXct(
      dateTime, tz = 'GMT', format = '%Y-%m-%dT%H:%M:%S%z'
      ) + 3600 * offset] %>%
    .[, .(lat = latitude, lon = longitude, time)] %>% as.data.frame
}

#' Read one page of SPOT data
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
spot_try <- function(url) {
  try({
    resp <- jsonlite::fromJSON(url)
    while (class(resp) ==  'try-error') {
      Sys.sleep(2)
      resp <- jsonlite::fromJSON(url)
    }
  })

  if (!is.null(resp$response$errors)) {
    stop(resp$response$errors$error$description)
  } else {
    msgs <- resp$response$feedMessageResponse$messages$message
  }

  spot_cols(msgs)
}

#' Read multiple pages of SPOT data
#'
#' @param id
#' @param all
#' @param password
#'
#' @return
#' @export
#'
#' @examples
spot_build <- function(id, all = FALSE, password = NULL) {
  # get first page of up to 50 trackpoints
  d <- spot_try(spot_url(id, password = password))

  if (all) {  # check for additional pages of data
    Sys.sleep(2)  # SPOT API rate limit
    n <- 0
    repeat({
      n <- n + 50
      next50 <- try(
        spot_try(spot_url(id, start = n, password)),
        silent = TRUE
      )
      if (class(next50) != 'try-error')
        d <- rbind(d, next50)  # merge new data
      else break
      Sys.sleep(2)
    })  # END REPEAT
  }  # END IF

  d[order(d$time), ]  # sort by timestamp
}
