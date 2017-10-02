cr_score <- function(DT, response, models, by) {
  library(Metrics)
  # check if DT is a data.table
  if (!is.data.table(DT)) {
    setDT(DT)
  }
  
  output <- list()
  for (i in models) {
    res <- DT[, .(rmse(get(response), get(i))), by = by] 
    setnames(res, 2, i)
    
    output[[length(output) + 1]] <- res
  }
  
  output <- Reduce(cbind, output)
  return(output[, !duplicated(names(output)), with = F])
}
