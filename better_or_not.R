better_or_not <- function(results, label, prediction, old_prognosis, grouping) {
  output <- results[, c(label, prediction, old_prognosis, grouping), with = FALSE]
  
  output[, diff_old := abs(get(label) - get(old_prognosis))]
  output[, diff_new := abs(get(label) - get(prediction))]
  
  # Better or not?
  output[, Better := ifelse(diff_old > diff_new, "Yes", "No")]
  
  # summarise data
  by <- c(grouping, "Better")
  output2 <- output[, .N, by = by]
  
  # convert to wide format
  output2 <- dcast(output2, paste(paste(grouping, collapse = " + "), "~", "..."), value.var = "N")
  
  # add proportions
  output2[, sum := No + Yes][, `Odds of being better` := round(Yes / sum, 2)][, sum := NULL]
  
  return(output2)
}
