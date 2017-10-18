channel_sum <- function(output, ZA_type) {

  # create a smaller dataset
  namen <- c("Channel", "Date", "Date2", "id", ZA_type)
  dt <- output[, ..namen]
  
  # convert posixct to numeric
  dt[, Date2 := as.numeric(Date2)]
  
  # split data on month
  dt[, Weeknummer := paste(isoweek(Datum), lubridate::year(Datum), sep = "-")]
  dt <- split(dt, by = "Weeknummer")
  
  library(doParallel)
  # build cluster based on size of ZA_type and available cores
  no_cores <- detectCores() - 1
  no_groep <- 1:length(dt)
  
  if (length(no_groep) < no_cores) {
    cl <- makeCluster(no_groep, "FORK") # FORK
  } else {
    cl <- makeCluster(no_cores, "FORK")
  }

  registerDoParallel(cl)
  
  result <- list()
  for (j in ZA_type) {
    namen2 <- c("Channel", "Date", "Date2", "id", j)
    a <- foreach(i = no_groep, .packages = "data.table", .verbose = TRUE, .export = "namen2") %dopar% {
      DT <- dt[[i]][, ..namen2]
        res <- DT[, Sum := DT[DT[row,
                               .(Datum, Zender = setdiff(Channels, Zender), Datum2)],
                            on = .(Datum, Zender, Datum2), roll = 'nearest',
                            sum(get(j))]
                , by = .(row = 1:nrow(DT))]
      
      return(res)
    }
    result[[length(result) + 1]] <- rbindlist(a)
  }
  
  stopCluster(cl)
  return(result)
}
