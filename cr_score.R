cr_score <- function(DT, response, models, by) {
  library(Metrics)
  FUN <- match.fun(FUN)
  # check if DT is a data.table
  if (!is.data.table(DT)) {
    setDT(DT)
  }
  
  # save number of groups
  no_group <- length(by) + 1
  
  # calculate rmse per model
  output <- list()
  for (i in models) {
    res <- DT[, .(FUN(get(response), get(i))), by = by] 
    setnames(res, no_group, i)
    
    output[[length(output) + 1]] <- res
  }
  
  # reduce list output to data.table
  output <- Reduce(cbind, output)
  return(output[, !duplicated(names(output)), with = F])
}
