replace_by_fun <- function(DT, replace, FUN, response, grouping, round = NULL) {
  suppressPackageStartupMessages(library(data.table))
  FUN <- match.fun(FUN)
  
  # convert tot data.table if needed
  if(!is.data.table(DT)) {
    setDT(DT)
  }
  
  # calculate replacement values and join with original data
  a <- DT[, .(FUN(get(response))), by = grouping]
  DT <- merge(DT, a, by = grouping)
  
  # remove old variable and rename new one
  DT[, (replace) := NULL]
  setnames(DT, "V1", replace)
  
  # rounding
  if (is.null(round)) {
    return(DT)
  } else {
    DT[, (replace) := round(get(replace), digits = round)]
    return(DT)
  }

}
