channel_sum <- function(output, ZA_type) {

  # create a smaller dataset
  namen <- c("Channel", "Date", "Date2", "id", ZA_type)
  dt <- output[, ..namen]
  
  # convert posixct to numeric
  dt[, Date2 := as.numeric(Date2)]
  
  # split data on month
  dt[, Maand := zoo::as.yearmon(Datum)]
  dt <- split(dt, by = "Maand")
  
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
    a <- foreach(i = no_groep, .packages = c("sqldf", "data.table"), .verbose = TRUE, .export = "namen2") %dopar% {
      DT <- dt[[i]][, ..namen2]
      
      s <- sprintf("select id, sum(bZA) %s from (select a.*, b.%s bZA, min(abs(a.Date2 - b.Date2)) from DT a join DT b on a.Date = b.Date and a.Channel != b.Channel group by a.id, b.Channel) group by id",
                   paste0("Sum_", j), j)
      res <- sqldf(x = s, verbose = TRUE, dbname = tempfile())
      sqldf()
      return(res)
    }
    result[[length(result) + 1]] <- rbindlist(a)
  }
  
  stopCluster(cl)
  return(result)
}
