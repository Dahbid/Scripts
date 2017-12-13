update <- function(original_data, upDT, cols, id) {
  suppressPackageStartupMessages(library(data.table))
  if (!is.data.table(original_data)) {
    setDT(original_data)
  }
  if (!is.data.table(updated_data)) {
    setDT(original_data)
  }
  
  if (!columns_to_update %in% names(DT) & !columns_to_update %in% names(updated_data)) {
    stop("Column name(s) not found in one or both given data.tables.")
  }
  
  setkeyv(original_data, id)
  setkeyv(updated_data, id)
  original_data[updated_data, (columns_to_update) := setDT(mget(paste0('i.', columns_to_update)))]
  
  setkey(original_data, NULL)
  setkey(updated_data, NULL)
}
