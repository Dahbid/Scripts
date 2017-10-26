cr_score <- function(DT, label, models, by, FUN) {
  library(Metrics)
  library(Troep)
  library(ggplot2)
  
  #input quoten
  label <- deparse(substitute(label))
  by = deparse(substitute(by))
  FUN <- deparse(substitute(FUN))
  
  huh <- FUN
  FUN <- match.fun(FUN)
  
  # check if DT is a data.table
  if (!is.data.table(DT)) {
    setDT(DT)
  }
  
  # save number of groups
  no_group <- length(by) + 1
  
  # calculate rmse per model
  output <- list()
  for (i in models) {
    res <- DT[, .(FUN(get(label), get(i))), by = by] 
    setnames(res, no_group, i)
    
    output[[length(output) + 1]] <- res
  }
  
  # reduce list output to data.table
  output <- Reduce(cbind, output)
  
  if (length(models) > 1) {
    output <- output[, !duplicated(names(output)), with = FALSE]
  }
  
  output2 <- melt(output, id.vars = by)
  # plot output2
  p <- ggplot(data = output2,
         mapping = aes(x = get(by), y = value, fill = variable, label = round(value, 2))) +
    geom_bar(stat = "identity", position = "dodge") + 
    geom_text(position = position_dodge(0.9), vjust = 2) +
    labs(x = by, y = "Score", title = huh) +
    guides(fill = guide_legend(title = "Model")) +
    theme(legend.position = "top")
  
  print(p)
  return(output)
}
