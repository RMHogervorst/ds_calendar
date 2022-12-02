library(calendar)
library(logger)
log_threshold(INFO)


#### Functions ###
# some calendars are empty and this gives a problem.
parse_calendar <- function(url) {
  #
  for (attempt_i in seq_len(5)) {
    res <- curl::curl_fetch_disk(url, tempfile())
    if (res$status_code == 504) { # deal with timeouts.
      backoff <- runif(n = 1, min = 0, max = 2^attempt_i - 1)
      log_info("Backing off for {round(backoff,2)} seconds for {url}")
      Sys.sleep(backoff)
    } else {
      log_debug("Parsing {url}")
      if (any(grepl("BEGIN:VEVENT", readLines(res$content, warn = FALSE)))) {
        result <- ic_read(res$content)
      } else {
        log_info("no events in {url}")
        result <- data.frame()
      }
      break
    }
  }
  result
}

replace_insert <- function(out, what, replacement) {
  start <- grep("BEGIN:VCALENDAR", out)
  end <- grep("BEGIN:VTIMEZONE", out)
  loc <- grep(what, out)
  if (length(loc) > 0) {
    if (start < loc && loc < end) {
      out[loc] <- paste0(what, replacement)
    }
  }
  out
}

###### actual work
calendars <- read.csv("calendars.csv")
log_info("found {nrow(calendars)} calendars")

all_calendar_content <- purrr::map_dfr(calendars$address, parse_calendar)

if (nrow(all_calendar_content) == 0) {
  stop("No events found or something went wrong", call. = FALSE)
}
cleaned_up <- all_calendar_content |>
  dplyr::distinct(`DTSTART;TZID=Europe/Amsterdam`, SUMMARY, CREATED, LOCATION, .keep_all = TRUE) |>
  dplyr::arrange(`DTSTART;TZID=Europe/Amsterdam`)
log_info("found {nrow(cleaned_up)} events")

# ic_write(cleaned_up, "ds_events.ics") # I'd like to add some details to the file that do not yet exist
out <- ic_character(cleaned_up)
log_info("changing meta information in calendar")
out |>
  replace_insert("PRODID:", "-//DATA//SCIENCE//MEETUPS//NL") |>
  replace_insert("X-ORIGINAL-URL:", "https://github.com/RMHogervorst/ds_calendar") |>
  replace_insert("X-WR-CALNAME:", "Dutch Data Science Calendar") |>
  writeLines("ds_events.ics")
