multiplier <- function(train = NULL, test, from, to, by, label, prediction) {
  suppressPackageStartupMessages(library(data.table))
  
  # input mappen
  # label <- deparse(substitute(label))
  # prediction <- deparse(substitute(prediction))
  
  # range creëren 
  multiplier_sequence <- seq(from, to, by)
  multiplier_sequence <- multiplier_sequence[multiplier_sequence != 1]
  
  if (!is.null(train)) {
    # for loop in train
    result_train <- vector("list", length(multiplier_sequence))
    for (i in multiplier_sequence) {
      # index <- grep(paste0(i, '$'), multiplier_sequence)
      index <- paste0('X', i)
      DT <- train[, c(label, prediction), with = FALSE]
      
      # multiplier toepassen
      DT[, test := i * get(prediction)]
      
      # rmse berekenen
      res <- DT[, .(i = Metrics::rmse(get(label), test))]
      setnames(res, "i", paste0("X", i))
      result_train[[index]] <- res
    }
    
    result_train <- as.data.table(result_train)
    result_train[, origineel := Metrics::rmse(train[[label]], train[[prediction]])]
  }
  
  
  # for loop in test
  result_test <- vector("list", length(multiplier_sequence))
  names(result_test) <- paste0("X", multiplier_sequence)
  for (j in multiplier_sequence) {
    # index <- grep(paste0(j, '$'), multiplier_sequence)
    index <- paste0('X', j)
    DT <- test[, c(label, prediction), with = FALSE]
    
    # multiplier toepassen
    DT[, test := j * get(prediction)]

    # rmse berekenen
    res <- DT[, .(j = Metrics::rmse(get(label), test))]
    setnames(res, "j", paste0("X", j))
    result_test[[index]] <- res
  }
  
  result_test <- as.data.table(result_test)
  setnames(result_test, paste0('X', multiplier_sequence))
  result_test[, origineel := Metrics::rmse(test[[label]], test[[prediction]])]
  
  if (!is.null(train)) {
    # train en test resultaten samenvoegen
    result <- rbindlist(list(result_train, result_test))
  } else {
    result <- result_test
  }
  
  # beste gemiddelde score voor elke waarde van multiplier_sequence
  beste <- which.min(result)
  winnaar <- names(beste)
  
  # omzetten naar getal
  if (winnaar == "origineel") {
    winnaar = 1
  } else {
    winnaar <- as.numeric(sub('X', '', winnaar))
  }
  return(winnaar)
}
