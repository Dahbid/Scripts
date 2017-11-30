descriptives <- function(DT) {
  namen <- names(DT)[sapply(DT, class) %in% c("character", "numeric", "integer", "factor")]
  wut <- DT[, .(Variable = namen, 
                  `Empty Cells` = sapply(.SD, function(x) sum(x == "", na.rm = TRUE))), .SDcols = namen]
  
  
  resultaat <- DT[, .(Variable = names(DT), 
                        Rows = .N, 
                        Missing = sapply(.SD, function(x) sum(is.na(x))),
                        Zeroes = sapply(.SD, function(x) sum(x == 0, na.rm = T)),
                        `Infinite Values` = sapply(.SD, function(x) sum(is.infinite(x))),
                        `Distinct Values` = sapply(.SD, function(x) length(unique(x))),
                        Type = sapply(.SD, function(x) class(x)[[1]]))][, ':=' (`Perc. Missing` = round(Missing / Rows, 2),
                                                                                `Perc. Zero` = round(Zeroes / Rows, 2),
                                                                                `Perc. Infinite` = round(`Infinite Values` / Rows, 2))]
  
  huh <- merge(resultaat, wut, by = "Variable", all.x = TRUE, sort = FALSE)
  huh[is.na(`Empty Cells`), `Empty Cells` := 0][, `Perc. Empty` := `Empty Cells` / Rows]
  
  
  setcolorder(huh, c("Variable", "Rows", "Missing", "Perc. Missing", "Zeroes", "Perc. Zero", "Empty Cells", "Perc. Empty", 
                     "Infinite Values", "Perc. Infinite", "Distinct Values", "Type"))
  return(huh)
}
