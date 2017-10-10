date_extract <- function(DT, date) {
  suppressPackageStartupMessages(library(data.table))
  date <- deparse(substitute(date))
  
  if ( lubridate::is.Date(DT[[date]]) ) {
    # year, semester, quarter, month, week, isoweek, day, dayinmonth
    DT[, ':=' (year = lubridate::year(get(date)),
               leapyear = lubridate::leap_year(get(date)),
               semester = lubridate::semester(get(date)),
               quarter = lubridate::quarter(get(date)),
               month = lubridate::month(get(date)),
               week = lubridate::week(get(date)),
               isoweek = lubridate::isoweek(get(date)),
               day = lubridate::day(get(date)),
               weekday = lubridate::wday(get(date)),
               monthday = lubridate::mday(get(date)))]
  } else if (lubridate::is.POSIXct(DT[[date]])) {
    DT[, ':=' (year = lubridate::year(get(date)),
               leapyear = lubridate::leap_year(get(date)),
               semester = lubridate::semester(get(date)),
               quarter = lubridate::quarter(get(date)),
               month = lubridate::month(get(date)),
               week = lubridate::week(get(date)),
               isoweek = lubridate::isoweek(get(date)),
               day = lubridate::day(get(date)),
               weekday = lubridate::wday(get(date)),
               monthday = lubridate::mday(get(date)),
               hour = lubridate::hour(get(date)),
               minute = lubridate::minute(get(date)),
               seconds = lubridate::second(get(date)),
               timezone = lubridate::tz(get(date)))]
  }
  
  return(DT)
}
