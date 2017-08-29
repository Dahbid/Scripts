cr_onehot <- function(predictors, matrix = TRUE, complete = TRUE) {
  # select names of factor variables
  factor_vars <- sapply(predictors, function(x) { length(levels(x))})
  factor_vars <- names(factor_vars[factor_vars > 0] )
  
  if (length(factor_vars) > 0) {
    for (i in factor_vars) {
      inds <- as.character(unlist(lapply(Filter(is.factor, predictors[ ,..i]), levels))) # hey it works
      if (complete == FALSE) { # if complete == FALSE create n-1 dummies
        inds <- inds[1:length(inds) - 1]
      }
      predictors[, (inds) := lapply(lapply(inds, function(x) .SD == x), as.numeric), .SDcols = i]
    }
    # remove old factor variables
    predictors[, (factor_vars) := NULL]
  }
  if (matrix == TRUE) { # if matrix == TRUE return a sparse matrix, otherwise return a data.table
    predictors <- as.matrix(predictors)
    predictors <- Matrix::Matrix(predictors)
  } 
  
  return(predictors)
}
