
#' Read data from the SPOT API
#'
#' @param id SPOT feed ID
#' @param all Fetch most recent 50 trackpoint (default) or all available
#' @param password SPOT feed password
#'
#' @return A data.frame with three columns: lat (latitude), lon (longitude),
#'  time (timestamp)
#'
#' @importFrom jsonlite fromJSON
#' @import dplyr
#'
#' @export
#'
#' @references \url{http://faq.findmespot.com/index.php?action=showEntry&data=69}
#' @examples
read_spot <- function(id, all = FALSE, password = NULL) {

  # grab the relevant columns from the full SPOT feed
  get_cols <- function(df) {
    df %>%
      select(lat = ends_with('latitude'),
             lon = ends_with('longitude'),
             time = ends_with('dateTime')) %>%
      mutate(time = as.character(time)) %>%
      mutate(offset = as.integer(substr(
        time, nchar(time) - 4, nchar(time) - 2))) %>%
      mutate(time = as.POSIXct(time, tz = 'GMT',
                               format = '%Y-%m-%dT%H:%M:%S%z') + 3600 * offset) %>%
      select(-offset)
  }

  # construct URL for SPOT API
  urlbase <- 'https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/'
  urltail <- '/message.json'
  url <- paste(urlbase, id, urltail, sep='')
  if (!is.null(password))
    url <- paste0(url, '?feedPassword=', password)

  resp <- fromJSON(url(url))
  if (!is.null(resp$response$errors))
    stop(resp$response$errors$error$description)
  else
    msgs <- as.data.frame(resp)

  count <- select(msgs, ends_with('totalCount'))[1, 1]
  pages <- (count - 1) %/% 50 + 1
  data <- get_cols(msgs)

  if (all == TRUE && pages > 1) {
    # a single page of SPOT tracks is 50 trackpoints;
    # figure out how many pages we need to download
    for (p in 2:pages) {
      cat(paste0("Downloading page ", p, "\n"))
      start <- 50 * (p - 1)
      startUrl <- paste('?start=', start, sep='')
      nextUrl <- paste(urlbase, id, urltail, startUrl, sep='')
      if (!is.null(password))
        nextUrl <- paste0(nextUrl, '&feedPassword=', password)
      nextMsgs <- fromJSON(nextUrl) %>% as.data.frame %>% get_cols
      # append pages X's data to page 1's data
      data <- rbind(data, nextMsgs)
    }
  }

  arrange(data, time)
}

#' Read data from DeLorme inReach API
#'
#' @param id inReach ID
#' @param date1 Start date
#' @param date2 End date
#'
#' @return A data.frame with three columns: lat (latitude), lon (longitude),
#'  time (timestamp)
#' @export
#'
#' @references \url{https://support.delorme.com/kb/articles/26-about-inreach-kml-feeds}
#' @examples
read_delorme <- function(id, date1, date2 = NULL) {
  urlhead <- 'https://share.delorme.com/feed/share/'
  d1 <- strftime(date1, '%Y-%m-%dT%H:%MZ')
  url <- paste0(urlhead, id, "?d1=", d1)
  if (!(is.null(date2))) {
    d2 <- strftime(date2, '%Y-%m-%dT%H:%MZ')
    url <- paste0(url, "&d2=", d2)
  }
  raw <- xml2::read_xml(url)

  process <- function(data, name) {
    xpath <- paste0(".//d1:Data[@name = '", name, "']")
    xml2::xml_find_all(data, xpath, xml_ns(raw)) %>%
      xml2::xml_text() %>% stringr::str_trim("both")
  }

  lat <- process(raw, "Latitude") %>% as.numeric
  lon <- process(raw, "Longitude") %>% as.numeric
  time <- process(raw, "Time UTC") %>% as.POSIXct(tz = 'GMT', format = '%m/%d/%Y %I:%M:%S %p')

  d <- data.frame(lat, lon, time)
  if (nrow(d) == 0) stop("no data")
  else d
}

read_traccar <- function(deviceid, start_time, stop_time,
                         db, host, port, user, password) {
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(RMySQL))

  db <- src_mysql(db, host, port, user, password)

  res <- as.data.frame(tbl(db, sql(
    "select uniqueid, devicetime, latitude, longitude
    from positions join devices on (positions.deviceid = devices.id)
    where accuracy = 0")) %>%
      filter(uniqueid == id, devicetime > start, devicetime < stop) %>%
      select(lat = latitude, lon = longitude, time = devicetime))

  if (nrow(res) == 0) stop("No rows available")
  else return(res)
}

#' Get timezone from lat/lon coordinates via Google Timezone API
#'
#' @param lat latitude (double)
#' @param lon longitude (double)
#' @param api_key Google Timezone API key
#'
#' @return timezone ID (character vector)
#' @export
#'
#' @examples
coord2tz <- function(lat, lon, api_key) {
  baseurl <- 'https://maps.googleapis.com/maps/api/timezone/json?location='
  url <- glue::glue("{baseurl}{lat},{lon}&timestamp=0&key={api_key}")
  jsonlite::fromJSON(url)$timeZoneId
}
