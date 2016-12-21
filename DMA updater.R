update_dma <- function() {
  system.time({
    #inlezen
    vec_ruw <- choose.files(caption = "Open het ruwe channel url report",
                            default = "S:\\Insights\\5 - Business & Data Solutions\\2. DMA\\Tableau\\Dashboard\\Data\\Channel URL\\Ruw\\ruw")
    resultaat <- list()
    resultaat2 <- vector()
    for (i in vec_ruw) {
    dma <- list()
    dma$document <- readxl::read_excel(path = i, sheet = 1, skip = 1, col_names = F)           # sheet 1
    dma$document <- tidyr::separate(dma$document, X4, c("X4a", "X4"), sep = " - ", fill = "left")   # brand en campagne scheiden
    dma$document <- dma$document[, apply(dma$document, 2, function(x) !any(is.na(x)))]       # lege kolom die ontstaat wegdoen
    dma$channel <- readxl::read_excel(path = i, sheet = 3, skip = 2, col_types = c(rep("text", 14)))           # sheet 2
    dma$channel <- dma$channel[,3:ncol(dma$channel)]
    
    #bewerken
    # variabelen Klant en Campaign toevoegen
    pattern <- paste0(dma$document[1,4], collapse = "|") 
    if (pmatch(pattern, dma$document[2,2], nomatch = 0) > 0) {
      dma$document[2,2] <- trimws(gsub(pattern = pattern, "", dma$document[2,2]))
    }
    dma$channel <- cbind(a=dma$document[1,4], b=dma$document[2,2], dma$channel)
    names(dma$channel)[1] <- "Klant"
    names(dma$channel)[2] <- "Campaign"
    
    # Filteren op Format en kolommen met 3s of 10s verwijderen
    dma$channel <- dma$channel[!is.na(dma$channel$Format),]
    namen <- c("Klant", "Campaign", "Network", "Placement", "Target", "Domain", "Format", "Impressions", "Measured",
               "Avg ToP", "Avg TiV", "> 5s", "> 1s")
    dma$channel <- dplyr::select(dma$channel, dplyr::one_of(namen))
	
    # start date en end date toevoegen (mutate voegt het aan het eind toe)
    dma$datum <- data.frame(dma$document[3,4])
    dma$datum <- tidyr::separate(dma$datum, X6, into = c("Start date", "End date"), sep = " - ")
    dma$channel <- dplyr::mutate(dma$channel, `Start date` = dma$datum$`Start date`, `End date` = dma$datum$`End date`)
    
    # omzetten in datum variabelen en het format aanpassen in dd-mm-jjjj
    dma$channel$`Start date` <- strptime(as.character(dma$channel$`Start date`), format = "%Y-%m-%d")
    dma$channel$`Start date` <- format(dma$channel$`Start date`, "%d-%m-%Y")
    dma$channel$`End date` <- strptime(as.character(dma$channel$`End date`), format = "%Y-%m-%d")
    dma$channel$`End date` <- format(dma$channel$`End date`, "%d-%m-%Y")
    
    #ehhh... ja
    dma$channel[8:13] <- lapply( dma$channel[8:13], function(col) as.numeric(gsub("-$|\\,", "", col)))
    # eeeennnnnn streepjes terug
    dma$channel[, 8:13][is.na(dma$channel[, 8:13])] <- "-"
    # wat als ik alle punten met komma's verwissel?
    dma$channel[8:13] <- lapply( dma$channel[8:13], function(col) gsub(".", ",", col, fixed = T))
    resultaat[[length(resultaat)+1]] <- dma
    
    # output naam en map creëren
    output_naam <- gsub(" \\(Ruw\\)", "", basename(i), ignore.case = T)
    
    resultaat2 <- append(resultaat2, output_naam)
    }
    vec_export <- choose.dir(caption = "Kies de opslagmap",
                             default = "S:\\Insights\\5 - Business & Data Solutions\\2. DMA\\Tableau\\Dashboard\\Data\\Channel URL")
    
    # samenvoegen en exporteren
    for (j in 1:length(vec_ruw)) {
      xlsx::write.xlsx(x = resultaat[[j]]$channel, file = paste0(vec_export, sep = "\\", resultaat2[j]), row.names = F)
    }
  })  
}
