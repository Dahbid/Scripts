cr_onehot_var <- function(predictors, factor, complete = TRUE) {
  suppressPackageStartupMessages(library(data.table))
  
  # check if the factor is in the DT and of class factor
  if (!factor %in% names(predictors)) {
    stop("The dataset does not contain a variable called: ", factor)
  } else if (is.data.table(predictors)) {
    is_DT <- TRUE
    if (!is.factor(predictors[, get(factor)])) {
      message("Given factor variable is not of class 'factor': Converting.")
      predictors[, (factor) := as.factor(get(factor))]
    }
  } else {
    is_DT <- FALSE
    if (!is.factor(predictors[, factor])) {
      message("Given factor variable is not of class 'factor': Converting.")
      predictors[, factor] <- as.factor(predictors[, factor])
    }
  }
  
  # convert to data.table if necessary
  if (!is_DT) {
    setDT(predictors)
  }
  index <- as.character(unlist(lapply(Filter(is.factor, predictors[ ,..factor]), levels)))
  if (!complete) {
    index <- index[1:length(index) - 1]
  }
  predictors[, (index) := lapply(lapply(index, function(x) .SD == x), as.numeric), .SDcols = factor]
  
  # remove old factor variable
  predictors[, (factor) := NULL]
  
  # if the input dataset was a dataframe, return it to that class
  if (!is_DT) {
    predictors <- as.data.frame(predictors)
    return(predictors)
  } else {
    return(predictors)
  }
}
