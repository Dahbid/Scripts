date_filter <- function(predictors, begin_date, end_date, date_variable, remove = FALSE) {
  suppressPackageStartupMessages(library(data.table))
  
  # check if predictors is a data.table
  if (!is.data.table(predictors)) {
    setDT(predictors)
  }
  
  # create vector of all possible dates between begin_date and end_date
  date_vec <- lubridate::as_date(sapply(seq(as.Date(begin_date), as.Date(end_date), by="days"),
                     function (x) format(x, "%Y-%m-%d")))
  
  if (remove) { # remove all data between begin_date and end_date
    if ( any(class(predictors[[date_variable]]) == "POSIXct") ) {
      return(predictors[!lubridate::as_date(get(date_variable)) %in% date_vec])
    } else {
      return(predictors[!get(date_variable) %in% date_vec])
    }
  } 
  else { # keep only the data between begin_date and end_date
    if ( any(class(predictors[[date_variable]]) == "POSIXct") ) {
      return(predictors[lubridate::as_date(get(date_variable)) %in% date_vec])
    } else {
      return(predictors[get(date_variable) %in% date_vec])
    }
  }
}
