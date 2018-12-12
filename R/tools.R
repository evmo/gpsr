filter_track_by_date <- function(track, date_filter) {
  dplyr::filter(track, as.Date(time) == date_filter)
}

filter_track_dir_by_date <- function(dir, date_filter) {
  dpath <- file.path(dir, "filt")
  if (dir.exists(dpath)) unlink(dpath, recursive = T)
  file_list <- list.files(dir)
  file_list <- gsub('.csv', '', file_list)
  dir.create(dpath)
  for (f in file_list) {
   d <- read_csv(file.path(dir, paste0(f, '.csv'))) %>%
     filter_track_by_date(date_filter)
   out_file <- file.path(dpath, paste0(f, '.csv'))
   write_csv(d, out_file)
  }
}
