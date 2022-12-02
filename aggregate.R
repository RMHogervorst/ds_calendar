library(calendar)
library(logger)
log_threshold(INFO)
calendars <- read.csv("calendars.csv")
log_info("found {nrow(calendars)} calendars")
# some calendars are empty and this gives a problem.
parse_calendar <- function(url){
  raw <- readLines(url)
  ic_list(raw)
}


all_calendar_content<- purrr::map(calendars$address, parse_calendar)
all_valid_urls <- calendars$address[lengths(all_calendar_content)>0]
log_info("found {length(all_valid_urls)} groups with events")
all_events <- purrr::map_dfr(all_valid_urls, ic_read)
log_debug("found {nrow(all_events)} events")
cleaned_up <-all_events |> 
  dplyr::distinct(`DTSTART;TZID=Europe/Amsterdam`, SUMMARY, CREATED, LOCATION, .keep_all = TRUE) |>
  dplyr::arrange(`DTSTART;TZID=Europe/Amsterdam`)
log_info("found {nrow(cleaned_up)} events")
ic_write(cleaned_up, "ds_events.ics")