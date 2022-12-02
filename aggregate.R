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

#ic_write(cleaned_up, "ds_events.ics") # I'd like to add some details to the file that do not yet exist
out <- ic_character(cleaned_up)
replace_insert <- function(out, what, replacement){
  start <- grep("BEGIN:VCALENDAR", out)
  end <- grep("BEGIN:VTIMEZONE", out)
  loc <- grep(what, out)
  if(length(loc)>0){
    if(start < loc && loc < end){
      out[loc] <- paste0(what, replacement)
    }
  }
  out
}
log_info("changing meta information in calendar")
out |>
  replace_insert("PRODID:","-//DATA//SCIENCE//MEETUPS//NL") |>
  replace_insert("X-ORIGINAL-URL:","https://github.com/RMHogervorst/ds_calendar") |>
  replace_insert("X-WR-CALNAME:","Dutch Data Science Calendar") |>
  writeLines( "ds_events.ics")
# PRODID
# X-ORIGINAL-URL:https://www.meetup.com/amsterdam-mlops-community/events/ical/
# X-WR-CALNAME:Events - Amsterdam MLOps Community
