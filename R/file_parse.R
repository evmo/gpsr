parseSource <- function(gpxFile) {
    xml2::read_html(gpxFile) %>%
        xml2::xml_node('gpx') %>%
        xml2::xml_attr('creator')
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
#' @param gpx_file
#'
#' @return data.frame
#' @export
#' @importFrom xml2 read_html xml_find_all xml_attr xml_text "%>%"
#'
#' @examples
read_gpx <- function(gpx_file) {
    msgs <- read_html(gpx_file)
    lat <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lat")
    lon <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lon")
    time <- xml_find_all(msgs, ".//trkseg//time") %>% xml_text

    time <- parseDateTime(time)

    if (is.null(time)) {
        return(NULL)
    } else {
        d <- data.frame(lat = as.numeric(lat),
                        lon = as.numeric(lon),
                        time,
                        stringsAsFactors = F)
        d[order(time, decreasing = T), ]
    }
}

#' Read TCX file
#'
#' @param tcx_file
#'
#' @return data.frame
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_ns xml_text "%>%"
#'
#' @examples
read_tcx <- function(tcx_file) {
    raw <- read_xml(tcx_file)

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
#' @return data.frame
#' @export
#'
#' @examples
read_spot_csv <- function(csv_file) {
    cc <- c(rep("character", 3), rep("numeric", 2), rep("character", 2))
    d <- read.csv(csv_file, header = F, colClasses = cc)[, c('V4', 'V5', 'V1')]
    names(d) <- c('lat', 'lon', 'time')
    d$time <- as.POSIXct(d$time, tz = 'GMT', format = '%m/%d/%Y %H:%M:%S')
    d[order(time, decreasing = T), ]
}
