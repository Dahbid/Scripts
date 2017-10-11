multiplier <- function(train, test, from, to, by, label, prediction) {
  suppressPackageStartupMessages(library(data.table))
  
  # input mappen
  label <- deparse(substitute(label))
  prediction <- deparse(substitute(prediction))
  
  # range creëren 
  multiplier_sequence <- seq(from, to, by)
  multiplier_sequence <- multiplier_sequence[multiplier_sequence != 1]

  # for loop in train
  result_train <- vector("list", length(multiplier_sequence))
  for (i in multiplier_sequence) {
    index <- grep(paste0(i, '$'), multiplier_sequence)
    DT <- train[, c(label, prediction), with = FALSE]
    
    # multiplier toepassen
    DT[, test := i * get(prediction)]
    
    # rmse berekenen
    res <- DT[, .(i = Metrics::rmse(get(label), test))]
    setnames(res, "i", paste0("X", i))
    result_train[[index]] <- res
  }
  
  result_train <- as.data.table(result_train)
  result_train[, origineel := Metrics::rmse(train[, label, with = FALSE], train[, prediction, with = FALSE])]
  
  # for loop in test
  result_test <- vector("list", length(multiplier_sequence))
  for (j in multiplier_sequence) {
    index <- grep(paste0(j, '$'), multiplier_sequence)
    DT <- test[, c(label, prediction), with = FALSE]
    
    # multiplier toepassen
    DT[, test := j * get(prediction)]
    
    # rmse berekenen
    res <- DT[, .(j = Metrics::rmse(get(label), test))]
    setnames(res, "j", paste0("X", j))
    result_test[[index]] <- res
  }
  
  result_test <- as.data.table(result_test)
  result_test[, origineel := Metrics::rmse(test[, label, with = FALSE], test[, prediction, with = FALSE])]
  
  # train en test resultaten samenvoegen
  result <- rbindlist(list(result_train, result_test))
  
  # beste gemiddelde score voor elke waarde van multiplier_sequence
  beste <- as.data.table(as.list(colMeans(result)))
  winnaar <- beste[, best := colnames(beste)[apply(beste, 1, which.min)]][, best]
  
  return(winnaar)
}
