mae2 <- function(preds, predictors) {
  labels <- getinfo(predictors, "label")
  f <- 2  
  x <-  preds-labels
  grad <- f*x / (abs(x)+f)
  hess <- f^2 / (abs(x)+f)^2
  return(list(grad = grad, hess = hess))
}
