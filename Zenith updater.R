update_zenith <- function() {
  system.time({
    Sys.setenv(R_ZIPCMD= "C:\\Rtools\\bin\\zip.exe") 
    # de meest recente altair data selecteren
    vec_zo <- choose.files(caption = "Open de meest recente ZO uren",
                           default = "S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Zenith\\Data\\ZO")
    vec_types <- c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",
                   "numeric", "numeric", "numeric", "numeric", "text", "numeric", "text", "text", "text",
                   "date", "date", "date", "date", "date", "text", "text", "text", "text", "text", "text", "text",
                   "text", "text", "text", "text", "text", "text", "text")
    
    # data inlezen altair
    ZO <- readxl::read_excel(path = vec_zo, col_types = vec_types)
    ZO$PTalent_ID <- as.numeric(ZO$PTalent_ID)
    ZO$CO_Document_Number <- as.numeric(ZO$CO_Document_Number)
    ZO$Hour_Status_code <- as.numeric(ZO$Hour_Status_code)

    # overige data inlezen
    # bron_uurtarief <- read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Zenith\\Urendashboard Zenith Werkversie.xlsx", sheet = 2)
    bron_lcodes <- readxl::read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Zenith\\Urendashboard Zenith Werkversie.xlsx", sheet = 3)
    bron_ptalentcodes <- readxl::read_excel("S:\\Insights\\5 - Business & Data Solutions\\10. Starcom Tableau Server DB\\Uren Dashboard\\Zenith\\Urendashboard Zenith Werkversie.xlsx", sheet = 2)

    # Excel formules vervangen
    output <- dplyr::select(ZO, 1:9)
    output <- dplyr::left_join(output, bron_ptalentcodes, by = c("PTalent_ID" = "P-Talent Code"))
    
    output$Work_Code <- ZO$Work_Code
    output <- dplyr::left_join(output, bron_lcodes, by = c("Work_Code" = "Wcode Ref"))
    output <- cbind(output, ZO[,11:28])
    
    # Posixct omzetten in date
    output[ , 23:27] <- lapply(output[,23:27], as.Date)
    
    # overplaatsen in een list voor de export
    output_list <- list("Uren Zenith" = output, "P-Talent" = bron_ptalentcodes, "L-Codes" = bron_lcodes)
    # exporteren naar excel
    openxlsx::write.xlsx(output_list, "C:\\Users\\davgies\\Documents\\R\\Projecten\\Urendashboard\\Urendashboard Zenith Werkversie.xlsx")
  })
}
