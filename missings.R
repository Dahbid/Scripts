missings <- function(DT, ordered = FALSE) {
  suppressPackageStartupMessages(library(data.table))
  # transform vectors into data.tables
  if (is.vector(DT)) {
    DT <- data.table(DT)
  }
  
  rown <- nrow(DT)
  if (is.data.table(DT)) {
    # find sum of missings
    DT <- DT[, lapply(.SD, function(x) sum(is.na(x)))]
    
    if (any(duplicated(names(DT)))) {
      setnames(DT, make.names(names(DT), unique = TRUE))
      warning("Dataset contains duplicated column names.")
    }
    # make it more readable and sort variables alphabetically
    DT <- melt(DT, measure.vars = names(DT), variable.name = "Variabele", value.name = "Missing")
    
    # get proportions
    DT[, Proportioneel := round(Missing / rown, 3)][]
    
    if (ordered) {
      DT <- DT[order(as.character(Variabele))]
    }
                      
    return(DT)
  } else {
    DT2 <- as.data.table(DT)
    DT2 <- DT2[, lapply(.SD, function(x) sum(is.na(x)))]
    if (any(duplicated(names(DT2)))) {
      setnames(DT2, make.names(names(DT2), unique = TRUE))
    }
    DT2 <- melt(DT2, measure.vars = names(DT2), variable.name = "Variabele", value.name = "Missing")[order(as.character(Variabele))]
    DT2[, Proportioneel := round(Missing / rown, 3)][]
    if (ordered) {
      DT2 <- DT2[order(as.character(Variabele))]
    }
    
    return(DT2)
  }

}
