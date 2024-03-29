#' Grab relevant columns from data-frame of SPOT API call
#'
#' @param df
#' @importFrom dplyr select mutate filter arrange
#'
#' @return
#'
#' @examples
spot_munge <- function(df) {
  df |>
    filter(
      messageType %in% c("TRACK", "EXTREME-TRACK", "UNLIMITED-TRACK"),
      dplyr::between(latitude, -180, 180)
    ) |>
    select(
      lat = latitude,
      lon = longitude,
      time = dateTime
    ) |>
    mutate(
      time = as.character(time),
      offset = as.integer(
        substr(time, nchar(time) - 4, nchar(time) - 2)
      ),
      time = as.POSIXct(
        time,
        tz = 'GMT',
        format = '%Y-%m-%dT%H:%M:%S%z'
      ) + 3600 * offset
    ) |>
    select(-offset) |>
    arrange(time)
}

#' Read single page of data from the SPOT API
#'
#' @param url SPOT API URL
#'
#' @return A data.frame with three columns: lat (latitude), lon (longitude),
#'  time (timestamp)
#'
#' @importFrom dplyr as_tibble
spot_page <- function(url) {
  resp <- jsonlite::fromJSON(url(url))

  if (!is.null(resp$response$errors))
    stop(resp$response$errors$error$description)

  as_tibble(resp$response$feedMessageResponse$messages$message) |>
    spot_munge()
}

#' Read data from the SPOT API
#'
#' @param id SPOT feed ID
#' @param all Fetch most recent 50 trackpoint (default) or all available
#' @param password SPOT feed password
#'
#' @return A tibble with three columns: lat (latitude), lon (longitude),
#'  time (timestamp)
#'
#' @importFrom dplyr arrange as_tibble filter
#'
#' @export
#'
#' @references \url{http://faq.findmespot.com/index.php?action=showEntry&data=69}
#' @examples
read_spot <-
  function(feed_id,
           to = NULL,
           from = NULL,
           all = FALSE,
           password = NULL) {

  url <- sprintf(
    "https://api.findmespot.com/spot-main-web/consumer/rest-api/2.0/public/feed/%s/message.json",
    feed_id
  )

  if (!is.null(password))
    url <- paste0(url, '?feedPassword=', password)

  resp <- jsonlite::fromJSON(url(url))

  if (!is.null(resp$response$errors))
    stop(resp$response$errors$error$description)

  count <- resp$response$feedMessageResponse$totalCount
  pages <- (count - 1) %/% 50 + 1

  data <- as_tibble(resp$response$feedMessageResponse$messages$message) |>
    spot_munge()

  if (all == TRUE && pages > 1) {
    # a single page of SPOT tracks is 50 trackpoints;
    # figure out how many pages we need to download
    for (p in 2:pages) {
      cat(paste0("Downloading page ", p, "\n"))
      start_n <- 50 * (p - 1)
      nextUrl <- glue::glue("{url}?start={start_n}")
      nextMsgs <- spot_page(nextUrl)
      # append pages X's data to page 1's data
      data <- rbind(data, nextMsgs)
    }
  }

  arrange(data, time) |>
    filter(time > from, time < to)

}

#' Process inReach XML API response
#'
#' @param xml
#' @param attrib
#'
#' @return
#' @importFrom xml2 xml_find_all xml_ns xml_text
process_inreach_xml <- function(xml, attrib) {
  xml_find_all(
    xml,
    xpath = paste0(".//d1:Data[@name = '", attrib, "']"),
    ns = xml_ns(xml)
  ) |>
    xml_text() |>
    stringr::str_trim("both")
}

#' Read data from Garmin inReach API
#'
#' @param from dttm
#' @param to dttm
#' @param id inReach ID
#'
#' @return A data.frame with three columns: lat (latitude), lon (longitude),
#'  time (timestamp)
#' @export
#' @importFrom glue glue
#'
#' @references  \url{https://support.garmin.com/en-US/?faq=tdlDCyo1fJ5UxjUbA9rMY8}
#' @examples
read_inreach <- function(id, from, to = NULL) {
  url <- glue(
    "https://share.garmin.com/feed/share/{id}",
     "?d1={format(as.POSIXct(from), '%Y-%m-%dT%H:%MZ')}")

  if (!(is.null(to)))
    url <- glue("{url}&d2={format(as.POSIXct(to), '%Y-%m-%dT%H:%MZ')}")

  raw <- xml2::read_xml(url)

  dplyr::tibble(
    lat = process_inreach_xml(raw, "Latitude") |> as.numeric(),
    lon = process_inreach_xml(raw, "Longitude") |> as.numeric(),
    time = process_inreach_xml(raw, "Time UTC") |>
      as.POSIXct(tz = 'GMT', format = '%m/%d/%Y %I:%M:%S %p')
  )
}

#' @rdname read_inreach
#' @export
read_delorme <- read_inreach

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

#' Read YB Tracking
#'
#' @param event_id
#'
#' @return tibble, 3 cols: lat, lon, time
#' @importFrom dplyr mutate rename select '%>%'
#' @export
#'
#' @examples
read_yb <- function(cust_id, event_id) {
  url_tpl <- 'https://app.yb.tl/APIX/Blog/GetPositions?keyword=%s&event=%s'
  url <- sprintf(url_tpl, cust_id, event_id)
  jsonlite::fromJSON(url)$positions %>%
    mutate(time = anytime::anytime(at / 1000)) %>%
    rename(lat = t, lon = g) %>%
    select(lat, lon, time)
}

#' Read Garmin Connect
#'
#' @param activity
#'
#' @return
#' @importFrom dplyr select mutate '%>%'
#' @export
#'
#' @examples
read_garmin_connect <- function(activity_id, code) {
  url_tpl <- 'https://connect.garmin.com/modern/proxy/activity-service/activity/%s/details?_=%s'
  url <- sprintf(url_tpl, activity_id, code)
  jsonlite::fromJSON(url)$geoPolylineDTO$polyline %>%
    select(lat, lon, time) %>%
    mutate(time = anytime::anytime(time / 1000))
}

#' Read from MarineTraffic historical positions API
#'
#' @param api_key
#' @param mmsi
#' @param from_date YYYY-MM-DD HH:MM:SS
#' @param to_date YYYY-MM-DD HH:MM:SS
#' @param period "hourly" or "daily"
#'
#' @return tbl
#' @export
#'
#' @examples
read_MT_hist <-
  function(api_key, mmsi,
           from_date, to_date,
           period = NULL,
           outfile) {
    baseurl <-
      'https://services.marinetraffic.com/api/exportvesseltrack/v:3'
    url <-
      glue::glue(
        '{baseurl}/{api_key}/mmsi:{mmsi}/fromdate:{from_date}/todate:{to_date}/protocol:csv'
      )
    if (!is.null(period))
      url <- glue::glue("{url}/period:{period}/")

    readr::read_csv(utils::URLencode(url)) |>
      dplyr::select(lat = LAT, lon = LON, time = TIMESTAMP)
  }
