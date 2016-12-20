update_smartcontent <- function(platform) {
  system.time({
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
    smart1 <- readxl::read_excel(path = vec_ruw, sheet = 1)
    smart1$Starts <- as.Date(smart1$Starts)
    
    # eerste 2 kolommen verwijderen + Results Type (Result indicator)
    smart1 <- dplyr::select(smart1, -1, -2, -5)
    
    # na Campaign drie kolommen toevoegen: Platform, Ad type en Brand
    smart1$Platform <- ""
    smart1$`Ad type` <- ""
    smart1$Brand <- ""
    smart1 <- smart1[, c(1, 43:45, 2:42)]
    
    # Ad type opvullen met informatie uit de Campaign variabele
    smart1 <- tidyr::separate(smart1, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
    smart1$`Ad type` <- trimws(smart1$Campaign2)
    smart1 <- tidyr::unite(smart1, Campaign, 1:3, sep = "-")
    
    # brandnaam toevoegen aan Brand
    brandnaam <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                      x = basename(vec_ruw), "-", ignore.case = T))
    smart1$Brand <- brandnaam
    
    # Platform toevoegen vanuit functie-argument
    if (missing(platform)) {
      smart1$Platform <- ""
    } else {
      smart1$Platform <- platform
    }
    
    
    # extra dashboard variabelen berekenen en variabelennamen aanpassen aan dashboard document
    smart1 <- dplyr::mutate(smart1, 
             `Conversation Rate` = `Post comments`/Reach,
             `Amplification Rate` = `Post shares`/Reach,
             `Applause Rate` = `Post Reactions`/Reach,
             `Key Engagements` = (`Post comments`+`Post shares`+`Post Reactions`)/Reach)
    var_namen <- c("Campaign Name",	"Platform",	"Ad Type",	"Brand",	"Results",	"Reach",	"Amount Spent (EUR)",	"Clicks (All)",	
                   "Cost per Unique Click (All) (EUR)",	"CPC (All) (EUR)",	"CPM (Cost per 1,000 Impressions) (EUR)",
                   "Cost per 1,000 People Reached (EUR)",	"CTR (All)",	"Frequency",	"Impressions",	"Objective",	"Social Impressions",	
                   "Social Reach", "Page Likes",	"Page Engagement",	"Photo Views",	"Post Shares",	"Post Comments",	"Post Engagement",	
                   "Post Likes", "Clicks to Play Video",	"Video Views",	"Cost per Page Like (EUR)",	"Cost per Photo View (EUR)",	
                   "Cost per Post Share (EUR)", "Cost per Post Comment (EUR)",	"Cost per Post Engagement (EUR)",	"Cost per Post Like (EUR)",	
                   "Cost per Clicks to Play Video (EUR)", "Cost per Video View (EUR)",	"Avg. % of Video Viewed",	"Video Views to 100%", 
                   "Video Views to 25%",	"Video Views to 50%", "Video Views to 75%",	"Video Views to 95%",	"Starts",	"CPC (Link)",	"CTR (Link)",	
                   "Link Clicks",	"Conversation Rate", "Amplifcation Rate",	"Applause Rate", "Key Engagements")
    colnames(smart1) <- var_namen
    
    #(ruw met doelgroep)
    smart2 <- readxl::read_excel(path = vec_ruw_doel, sheet = 1)
    smart2$Starts <- as.Date(smart2$Starts)
    smart2 <- dplyr::select(smart2, -1, -2, -dplyr::ends_with("indicator"))
    smart2$Platform <- ""
    smart2$`Ad type` <- ""
    smart2$Brand <- ""
    smart2 <- smart2[, c(1, 45:47, 2:44)]
    if (missing(platform)) {
      smart2$Platform <- ""
    } else {
      smart2$Platform <- platform
    }
    smart2 <- tidyr::separate(smart2, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
    smart2$`Ad type` <- trimws(smart2$Campaign2)
    smart2 <- tidyr::unite(smart2, Campaign, 1:3, sep = "-")
    brandnaam2 <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                       x = basename(vec_ruw_doel), "-", ignore.case = T))
    smart2$Brand <- brandnaam2
    
    smart2 <- dplyr::mutate(smart2,
             `Conversation Rate` = `Post comments`/Reach,
             `Amplification Rate` = `Post shares`/Reach,
             `Applause Rate` = `Post Reactions`/Reach,
             `Key Engagements` = (`Post comments`+`Post shares`+`Post Reactions`)/Reach)
    var_namen2 <- c("Campaign Name",	"Platform", 	"Ad Type",	"Brand",	"Age",	"Gender",	"Results",	"Reach",	
                    "Amount Spent (EUR)",	"Clicks (All)",	"Cost per Unique Click (All) (EUR)",	"CPC (All) (EUR)",
                    "CPM (Cost per 1,000 Impressions) (EUR)",	"Cost per 1,000 People Reached (EUR)",	"CTR (All)",	
                    "Frequency",	"Impressions",	"Objective",	"Social Impressions",	"Social Reach",	"Page Likes",
                    "Page Engagement",	"Photo Views",	"Post Shares",	"Post Comments",	"Post Engagement",	"Post Likes",	
                    "Clicks to Play Video",	"Video Views",	"Cost per Page Like (EUR)",	"Cost per Photo View (EUR)",	
                    "Cost per Post Share (EUR)",	"Cost per Post Comment (EUR)",	"Cost per Post Engagement (EUR)",	
                    "Cost per Post Like (EUR)",	"Cost per Clicks to Play Video (EUR)",	"Cost per Video View (EUR)",	
                    "Avg. % of Video Viewed",	"Video Views to 100%",	"Video Views to 25%",	"Video Views to 50%",	
                    "Video Views to 75%",	"Video Views to 95%",	"Datum Post",	"CPC (Link)",	"CTR (Link)",	"Link Clicks",
                    "Conversation Rate",	"Amplifcation Rate",	"Applause Rate",	"Key Engagements")
    colnames(smart2) <- var_namen2
    
    #Exporteren
    #outputmap pad creëren
    outputmap <- gsub(x=dirname(dirname(vec_ruw) ),"\\/", "\\\\")
    
    #vec_ruw
    outputnaam1 <- paste0(outputmap, sep = "\\", gsub(" \\(Ruw\\)", "", basename(vec_ruw), ignore.case = T))
    #vec_ruw_doel
    outputnaam2 <- paste0(outputmap, sep = "\\", gsub("Ruw ", "", basename(vec_ruw_doel), ignore.case = T))
    
    # wegschrijven
    xlsx::write.xlsx(x = as.data.frame(smart1), file = outputnaam1, row.names = F)
    xlsx::write.xlsx(x = as.data.frame(smart2), file = outputnaam2, row.names = F)
  })
}
