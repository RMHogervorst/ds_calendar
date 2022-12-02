library(calendar)
library(logger)
log_threshold(INFO)
calendars <- read.csv("calendars.csv")
log_info("found {nrow(calendars)} calendars")
# some calendars are empty and this gives a problem.
parse_calendar <- function(url){
  #raw <- readLines(url) # this works but is a bit 
  for (attempt_i in seq_len(10)){
    resp <- httr::GET(url, httr::user_agent("http://github.com/rmhogervorst/ds_calendar"))
    if (httr::status_code(resp) == 504){
      backoff <- runif(n=1, min=0, max=2^attempt_i - 1)
      log_info("Backing off for {round(backoff,2)} seconds for {url}")
      Sys.sleep(backoff)
    }else{
      raw <- httr::content(resp)
      break
    }
  }
  ic_list(raw)
}


all_calendar_content<- purrr::map(calendars$address, parse_calendar)
all_valid_urls <- calendars$address[lengths(all_calendar_content)>0]
log_info("found {length(all_valid_urls)} groups with events")
all_events <- purrr::map_dfr(all_valid_urls, ic_read)
log_debug("found {nrow(all_events)} events")
if(length(all_valid_urls) == 0){stop('No events found or something went wrong', call. = FALSE)}
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
