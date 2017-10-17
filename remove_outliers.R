function(predictors, y, by) {
  suppressPackageStartupMessages(library(data.table))
  
  if (!is.data.table(predictors)) {
    setDT(predictors)
  }
  
  # bind predictors and y vector
  predictors <- cbind(predictors, y)
  
  # filter outliers
  predictors <- predictors[!(abs(y - median(y, na.rm = TRUE)) > 2 * sd(y, na.rm = TRUE)), .SD, by = by]
  
  # cleanup
  predictors[, y := NULL]
  
  return(predictors)
}
