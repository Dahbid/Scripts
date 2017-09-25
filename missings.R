missings <- function(DT) {
  rown <- nrow(DT)
  if (is.data.table(DT)) {
    # find sum of missings
    DT <- DT[, lapply(.SD, function(x) sum(is.na(x)))]
    
    # make it more readable and sort variables alphabetically
    DT <- melt(DT, measure.vars = names(DT), variable.name = "Variabele", value.name = "Missing")[order(as.character(Variabele))]
    
    # get proportions
    DT[, Proportioneel := round(Missing / rown, 3)][]
    
    return(DT)
  } else {
    DT2 <- as.data.table(DT)
    DT2 <- DT2[, lapply(.SD, function(x) sum(is.na(x)))]
    DT2 <- melt(DT2, measure.vars = names(DT2), variable.name = "Variabele", value.name = "Missing")[order(as.character(Variabele))]
    DT2[, Proportioneel := round(Missing / rown, 3)][]
    
    return(DT2)
  }

}
