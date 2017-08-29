missings <- function(DT) {
  rown <- nrow(DT)
  # find sum of missings
  DT <- DT[, lapply(.SD, function(x) sum(is.na(x)))]
  
  # make it more readable and sort variables alphabetically
  DT <- melt(DT, measure.vars = names(DT), variable.name = "Variabele", value.name = "Missing")[order(as.character(Variabele))]
  
  # get proportions
  DT[, Proportioneel := round(Missing / rown, 3)][]
}
