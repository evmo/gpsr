#' Parse creator of a GPX file
#'
#' @param gpxFile File path of the file to be parsed
#'
#' @return
#' @import xml2
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
gpx_creator <- function(gpxFile) {
    read_html(gpxFile) %>%
        xml_node('gpx') %>%
        xml_attr('creator')
}

parseDateTime <- function(datetimes) {
    dateRE <- c(
        # 2015-09-27 13:33:01  --> standard
        "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
        # 2015-09-27T13:33:01Z
        "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z",
        # 2014-10-20T22:45:38+11:00
        "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}{+,-}\\d{2}:\\d{2}",
        # 2014-04-08T13:03:08.000Z
        "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z",
        # "2014-10-02T17:05:07+0000
        "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}{+,-}\\d{4}"
    )

    if (grepl(dateRE[1], head(datetimes, 1))) {
        datetimes <- as.POSIXct(datetimes, tz = "GMT")
    } else if (grepl(dateRE[2], head(datetimes, 1))) {
        datetimes <- as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
    } else if (grepl(dateRE[3], head(datetimes, 1))) {
        tz <- as.numeric(substr(datetimes, nchar(datetimes) - 5, nchar(datetimes) - 3))
        datetimes <- substr(datetimes, 1, nchar(datetimes) - 6)
        datetimes <- as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") - tz * 3600
    } else if (grepl(dateRE[4], head(datetimes, 1))) {
        datetimes <- substr(datetimes, 1, nchar(datetimes) - 4)
        datetimes <- as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
    } else if (grepl(dateRE[5], head(datetimes, 1))) {
        tz <- as.numeric(substr(datetimes, nchar(datetimes) - 4, nchar(datetimes) - 2))
        datetimes <- substr(datetimes, 1, nchar(datetimes) - 5)
        datetimes <- as.POSIXct(datetimes, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT") - tz * 3600
    } else {
        datetimes <- NULL
    }

    return(datetimes)
}

#' Read GPX file
#'
#' @param gpxFile
#'
#' @return tibble
#' @export
#' @import xml2 tibble anytime
#' @importFrom magrittr "%>%"
#'
#' @examples
read_gpx <- function(gpxFile) {
    msgs <- read_html(gpxFile)
    lat <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lat")
    lon <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lon")
    time <- xml_find_all(msgs, ".//trkseg//time") %>% xml_text

    tibble(
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      time = anytime(time)
    ) %>% arrange(time)
}

#' Read TCX file
#'
#' @param tcx_file
#'
#' @return data.frame
#' @export
#' @import xml2
#' @importFrom magrittr "%>%"
#'
#' @examples
read_tcx <- function(tcxFile) {
    raw <- read_xml(tcxFile)

    lat <- raw %>% xml_find_all('.//d1:LatitudeDegrees', xml_ns(raw)) %>%
        xml_text %>% as.numeric
    lon <- raw %>% xml_find_all('.//d1:LongitudeDegrees', xml_ns(raw)) %>%
        xml_text %>% as.numeric
    time <- raw %>% xml_find_all('.//d1:Time', xml_ns(raw)) %>%
        xml_text %>% as.POSIXct(format = '%Y-%m-%dT%H:%M:%S.000Z')

    data.frame(lat, lon, time)
}

#' Read SPOT-exported track in CSV format
#'
#' @param csv_file
#'
#' @return tibble
#' @import readr anytime
#' @importFrom dplyr select rename mutate arrange '%>%'
#' @export
#'
#' @examples
read_spot_csv <- function(csv_file) {
    read_csv(csvFile, col_names = F) %>%
        select(X1, X4, X5) %>%
        rename(lat = X4, lon = X5, time = X1) %>%
        mutate(time = anytime(time)) %>%
        arrange(time)
}

#' Read SPOT JSON
#'
#' @param file_path
#'
#' @return
#' @importFrom purrr map_dbl map_chr pluck
#' @importFrom lubridate mdy_hms with_tz
#' @export
read_spot_json <- function(file_path, tz = "UTC") {
  positions <- jsonlite::read_json(file_path) |>
    pluck("d", "Assets", 1, "Positions")

  tibble::tibble(
    lat =  map_dbl(positions, ~ pluck(.x, "Lat")),
    lon =  map_dbl(positions, ~ pluck(.x, "Lng")),
    time = map_chr(positions, ~ pluck(.x, "Time")) |>
      mdy_hms(tz = tz) |>
      with_tz("UTC")
  ) |>
    dplyr::arrange(time)
}
