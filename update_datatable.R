update <- function(DT, upDT, cols, id) {
  suppressPackageStartupMessages(library(data.table))
  if (!is.data.table(DT)) {
    setDT(DT)
  }
  if (!is.data.table(upDT)) {
    setDT(DT)
  }
  
  if (!cols %in% names(DT) & !cols %in% names(upDT)) {
    stop("Column name(s) not found in one or both given data.tables.")
  }
  
  setkeyv(DT, id)
  setkeyv(upDT, id)
  DT[upDT, (cols) := setDT(mget(paste0('i.', cols)))]
  
  setkey(DT, NULL)
  setkey(upDT, NULL)
}
