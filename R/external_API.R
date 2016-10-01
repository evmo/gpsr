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
spot2csv <- function(id, all = F, fn = 'spotData', dir = NULL) {
  df <- read_spot(id, all)
  writepath <- paste0(dir, '/', fn, '.csv')
  write.csv(df, writepath, row.names = F)
}

# Grab KML feed from DeLorme shared page
read_delorme <- function(id, date1, date2 = NULL) {
  library(xml2); library(stringr)

  urlhead <- 'https://share.delorme.com/feed/share/'
  d1 <- strftime(date1, '%Y-%m-%dT%H:%MZ')
  url <- paste0(urlhead, id, "?d1=", d1)
  if (!(is.null(date2))) {
    d2 <- strftime(date2, '%Y-%m-%dT%H:%MZ')
    url <- paste0(url, "&d2=", d2)
  }
  raw <- read_xml(url)

  process <- function(data, name) {
    xpath <- paste0(".//d1:Data[@name = '", name, "']")
    xml_find_all(data, xpath, xml_ns(raw)) %>%
      xml_text %>% str_trim("both")
  }

  lat <- process(raw, "Latitude") %>% as.numeric
  lon <- process(raw, "Longitude") %>% as.numeric
  time <- process(raw, "Time UTC") %>% as.POSIXct(tz = 'GMT', format = '%m/%d/%Y %I:%M:%S %p')

  d <- data.frame(lat, lon, time)
  if (nrow(d) == 0) stop("no data")
  else d
}
