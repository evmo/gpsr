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
#' @importFrom xml2 read_html xml_find_all xml_attr xml_text
#' @importFrom magrittr "%>%"
#'
#' @examples
read_gpx <- function(gpxFile) {
    msgs <- read_html(gpxFile)
    lat <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lat")
    lon <- xml_find_all(msgs, ".//trkpt") %>% xml_attr("lon")
    time <- xml_find_all(msgs, ".//trkseg//time") %>% xml_text

    time <- parseDateTime(time)

    if (is.null(time)) {
        return(NULL)
    } else {
        data.frame(lat, lon, time, stringsAsFactors = F) %>%
            mutate(lat = as.numeric(lat)) %>%
            mutate(lon = as.numeric(lon)) %>%
            arrange(time)
    }
}

#' Read TCX file
#'
#' @param tcx_file
#'
#' @return data.frame
#' @export
#' @importFrom xml2 read_xml xml_find_all xml_ns xml_text
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
#' @return data.frame
#' @export
#'
#' @examples
read_spot_csv <- function(csvFile) {
    read.csv(csvFile, header = F, colClasses = c(
                rep("character", 3),
                rep("numeric", 2),
                rep("character", 2))) %>%
        select(-c(V2, V3, V6, V7)) %>%
        rename(lat = V4, lon = V5, time = V1) %>%
        mutate(time = as.POSIXct(time, tz = 'GMT',
                                format = '%m/%d/%Y %H:%M:%S')) %>%
        select(lat, lon, time) %>%
        arrange(time)
}
