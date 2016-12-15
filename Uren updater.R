update_urendashboard <- function() {
  system.time({
  require(openxlsx)
  require(readxl)
  require(dplyr)
  Sys.setenv(R_ZIPCMD= "C:\\Rtools\\bin\\zip.exe") 
  # de meest recente altair data selecteren
  vec_sc <- choose.files(caption = "Open de meest recente SC uren", default = "S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Data\\SC")
  vec_vx <- choose.files(caption = "Open de meest recente VX uren", default = "S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Data\\VX")
  vec_types <- c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",
                 "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text",
                 "date", "date", "date", "date", "date", "text", "text", "text", "text", "text", "text", "text",
                 "text", "text", "text", "text", "text", "text", "text")
  
  # data inlezen altair
  SC <- read_excel(path = vec_sc, col_types = vec_types)
  VX <- read_excel(path = vec_vx, col_types = vec_types)
  
  SC$PTalent_ID[SC$PTalent_ID=="94385"] <- "101710"  # Esther Buis heeft twee nummers
  SC$PTalent_ID <- as.numeric(SC$PTalent_ID)
  VX$PTalent_ID <- as.numeric(VX$PTalent_ID)
  SC$CO_Document_Number <- as.numeric(SC$CO_Document_Number)
  VX$CO_Document_Number <- as.numeric(VX$CO_Document_Number)
  SC$Hour_Status_code <- as.numeric(SC$Hour_Status_code)
  VX$Hour_Status_code <- as.numeric(VX$Hour_Status_code)
                 
  # overige data inlezen
  bron_uurtarief <- read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Bronbestand Urendashboard werkversie.xlsx", sheet = 2)
  bron_lcodes <- read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Bronbestand Urendashboard werkversie.xlsx", sheet = 3)
  bron_ptalentcodes <- read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Bronbestand Urendashboard werkversie.xlsx", sheet = 4)
  bron_xtra <- read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Bronbestand Urendashboard werkversie.xlsx", sheet = 5)
  
  # SC voorbereiden
  output <- SC %>% select(1:9)
  output <- left_join(output, bron_ptalentcodes, by = c("PTalent_ID" = "P-Talent Code"))
  
  output$Work_Code <- SC$Work_Code
  output <- left_join(output, bron_lcodes, by = c("Work_Code" = "Wcode Ref"))
  output <- cbind(output, SC[,11:28])
  
  # dan VX
  output2 <- VX %>% select(1:9)
  output2 <- left_join(output2, bron_ptalentcodes, by = c("PTalent_ID" = "P-Talent Code"))
  
  output2$Work_Code <- VX$Work_Code
  output2 <- left_join(output2, bron_lcodes, by = c("Work_Code" = "Wcode Ref"))
  output2 <- cbind(output2, VX[,11:28])
  
  # samenvoegen
  output <- rbind(output, output2)
  
  # electronic arts filter toepassen
  output <- output %>% 
    mutate(`Client Description` = replace(`Client Description`, WBS_Element_Description == "SC-EA - Electronic Arts", "Electronic Arts"),
           `Brand Descripton` = replace(`Brand Descripton`, WBS_Element_Description == "SC-EA - Electronic Arts", "Electronic Arts"))
  
  # SC- naar SC - Algemene uren 2016
  output <- output %>%
    mutate(WBS_Element_Description = replace(WBS_Element_Description, WBS_Element_Description == "SC-", "SC - Algemene uren 2016"))
  
  # nieuwe werknemers eraan plakken
  colnames(output) <- colnames(bron_xtra)
  output <- bind_rows(output, bron_xtra)
  output[ , 23:27] <- lapply(output[,23:27], as.Date)
  output$PTalent_ID[output$PTalent_ID==94385] <- 101710
  # overplaatsen in een list voor de export
  output_list <- list("Starcom Uren 2016" = output, "Uurtarief " = bron_uurtarief, "L-Codes" = bron_lcodes,
                      "P-Talentcodes" = bron_ptalentcodes, "xtra uren nieuwe WKNRS" = bron_xtra)
  # exporteren naar excel
  write.xlsx(output_list, "S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Bronbestand Urendashboard werkversie test.xlsx")
  })
}
