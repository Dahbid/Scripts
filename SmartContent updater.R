update_smartcontent <- function(platform) {
  system.time({
  require(readxl)
  require(dplyr)
  require(tidyr)  
  require(xlsx)
  
  #Inlezen
  vec_input <- choose.files(default = "S:\\Insights\\5 - Business & Data Solutions\\1. Data Visualisatie\\SmartContent\\Rapportages 2016\\ruw",
                            caption = "Selecteer de twee exportbestanden.")
  
  if (grepl(x = vec_input[1], pattern = "met doelgroep") == TRUE) {
    vec_ruw_doel <- vec_input[1]
    vec_ruw <- vec_input[2]
  } else {
    vec_ruw_doel <- vec_input[2]
    vec_ruw <- vec_input[1]
  }
  
    #Bewerken
    #(ruw)
    smart1 <- read_excel(path = vec_ruw, sheet = 1)
  
    # eerste 2 kolommen verwijderen + Results Type (Result indicator)
    smart1 <- select(smart1, -1, -2, -5)
  
    # na Campaign drie kolommen toevoegen: Platform, Ad type en Brand
    smart1$Platform <- ""
    smart1$`Ad type` <- ""
    smart1$Brand <- ""
    smart1 <- smart1[, c(1, 43:45, 2:42)]
  
    # Ad type opvullen met informatie uit de Campaign variabele
    smart1 <- separate(smart1, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
    smart1$`Ad type` <- trimws(smart1$Campaign2)
    smart1 <- unite(smart1, Campaign, 1:3, sep = "-")
  
    # brandnaam toevoegen aan Brand
    brandnaam <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                    x = basename(vec_ruw), "-", ignore.case = T))
    smart1$Brand <- brandnaam
  
    # Platform toevoegen vanuit functie-argument
    smart1$Platform <- platform
  
    #(ruw met doelgroep)
    smart2 <- read_excel(path = vec_ruw_doel, sheet = 1)
    smart2 <- select(smart2, -1, -2, -ends_with("indicator"))
    smart2$Platform <- ""
    smart2$`Ad type` <- ""
    smart2$Brand <- ""
    smart2 <- smart2[, c(1, 45:47, 2:44)]
    smart2$Platform <- platform
    smart2 <- separate(smart2, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
    smart2$`Ad type` <- trimws(smart2$Campaign2)
    smart2 <- unite(smart2, Campaign, 1:3, sep = "-")
    brandnaam2 <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                     x = basename(vec_ruw_doel), "-", ignore.case = T))
    smart2$Brand <- brandnaam2
  
  #Exporteren
  #outputmap pad creëren
  outputmap <- gsub(x=dirname(dirname(vec_ruw) ),"\\/", "\\\\")
  
  #vec_ruw
  outputnaam1 <- paste0(outputmap, sep = "\\", gsub(" \\(Ruw\\)", "", basename(vec_ruw), ignore.case = T))
  #vec_ruw_doel
  outputnaam2 <- paste0(outputmap, sep = "\\", gsub("Ruw ", "", basename(vec_ruw_doel), ignore.case = T))
  
  # wegschrijven
  write.xlsx(x = as.data.frame(smart1), file = outputnaam1, row.names = F)
  write.xlsx(x = as.data.frame(smart2), file = outputnaam2, row.names = F)
  })
}
