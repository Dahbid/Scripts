wape <- function(actual, predicted) {
  sum(abs(actual-predicted)) / sum(actual) * 100
}
