% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/external_API.R
\name{read_MT_hist}
\alias{read_MT_hist}
\title{Read from MarineTraffic historical positions API}
\usage{
read_MT_hist(api_key, mmsi, from_date, to_date, period = NULL, outfile)
}
\arguments{
\item{from_date}{YYYY-MM-DD HH:MM:SS}

\item{to_date}{YYYY-MM-DD HH:MM:SS}

\item{period}{"hourly" or "daily"}
}
\value{
tbl
}
\description{
Read from MarineTraffic historical positions API
}
