wape2 <- function(preds, predictors) {
  labels <-  getinfo(predictors, "label")
  err <- as.numeric(sum(abs(labels - preds)) / sum(labels) * 100)
  return(list(metric = "wape",value = err))
}
