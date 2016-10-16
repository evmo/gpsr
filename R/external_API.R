read_spot <- function(id, all = FALSE, password = NULL) {

  # grab the relevant columns from the full SPOT feed
  reduce <- function(df) {
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
    msgs <- resp %>% as.data.frame

  count <- select(msgs, ends_with('totalCount'))[1, 1]
  pages <- (count - 1) %/% 50 + 1
  data <- msgs %>% reduce

  if (all == TRUE && pages > 1) {
    # a single page of SPOT tracks is 50 trackpoints;
    # figure out how many pages we need to download
    for (p in 2:pages) {
      cat(paste0("Downloading page ", p, "\n"))
      start <- 50 * (p - 1) + 1
      startUrl <- paste('?start=', start, sep='')
      nextUrl <- paste(urlbase, id, urltail, startUrl, sep='')
      nextMsgs <- fromJSON(nextUrl) %>% as.data.frame %>% reduce
      # append pages X's data to page 1's data
      data <- rbind(data, nextMsgs)
    }
  }

  return(data %>% arrange(time))
}

# Scrape from API, write a CSV file
spot_to_csv <- function(id, all = F, fn = 'spotData', dir = NULL) {
  df <- read_spot(id, all)
  writepath <- paste0(dir, '/', fn, '.csv')
  write.csv(df, writepath, row.names = F)
}

# Grab KML feed from DeLorme shared page
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
      xml2::xml_text %>% stringr::str_trim("both")
  }

  lat <- process(raw, "Latitude") %>% as.numeric
  lon <- process(raw, "Longitude") %>% as.numeric
  time <- process(raw, "Time UTC") %>% as.POSIXct(tz = 'GMT', format = '%m/%d/%Y %I:%M:%S %p')

  d <- data.frame(lat, lon, time)
  if (nrow(d) == 0) stop("no data")
  else d
}

parseSource <- function(gpxFile) {
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