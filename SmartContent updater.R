library(readxl)
library(dplyr)
library(tidyr)
# inlezen
smart1 <- read_excel("WijzerInGeldzaken November Q4 2016 (Ruw).xlsx", sheet = 1)

# keuze vector
# ze kunnen dan twee ruwe bestanden invoeren. de naam wordt in een vector opgeslagen en vervolgens wordt op basis van een 
# if/else functie bepaald welk bestand welke soort bewerking krijgt. Slim.
vec_ruw <- choose.files(default = "S:\\Insights\\5 - Business & Data Solutions\\1. Data Visualisatie\\SmartContent\\Rapportages 2016\\ruw",
                        caption = "Selecteer de twee exportbestanden.")
# maand uit de
# eerste 2 kolommen verwijderen + Results Type (Result indicator)
smart1 <- select(smart1, -1, -2, -5)

# na Campaign drie kolommen toevoegen: Platform, Ad type en Brand
smart1$Platform <- ""
smart1$`Ad type` <- ""
smart1$Brand <- ""
smart1 <- smart1[, c(1, 43:45, 2:42)]

# platform moet ingevuld worden bij Platform
# kan niet, dat moet degene weten die de uitdraai maakt
# wordt argument in de functie
smart1$Platform <- platf

# voeg het type ad in dat te vinden is in de Campaign variabele
# in drieën delen en het middelste deel is de ad type
smart1 <- separate(smart1, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
smart1$`Ad type` <- trimws(smart1$Campaign2)
smart1 <- unite(smart1, Campaign, 1:3, sep = "-")

# vul het merk in. dit staat aan het begin van de bestandsnaam.
# is het altijd 1 woord? Neen.
# de maandnaam komt er altijd achter, maar die wisselt, al is die wel altijd gelijk aan de mapnaam

brandnaam <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                  x = basename(vec_ruw), "-", ignore.case = T))

# brandnaam toevoegen aan Brand
smart1$Brand <- brandnaam


# andere bestand
vec_ruw_doel <- choose.files(default = "S:\\Insights\\5 - Business & Data Solutions\\1. Data Visualisatie\\SmartContent\\Rapportages 2016\\ruw",
                             caption = "Selecteer de twee exportbestanden.")
smart2 <- read_excel(path = vec_ruw_doel, sheet = 1)
smart2 <- select(smart2, -1, -2, -ends_with("indicator"))

# na Campaign drie kolommen toevoegen: Platform, Ad type en Brand
smart2$Platform <- ""
smart2$`Ad type` <- ""
smart2$Brand <- ""
smart2 <- smart2[, c(1, 45:47, 2:44)]

# platform zelf invullen
smart1$Platform <- platf

# ad type invullen
smart2 <- separate(smart2, Campaign, into = c("Campaign1", "Campaign2", "Campaign3"), sep = "-", extra = "merge")
smart2$`Ad type` <- trimws(smart2$Campaign2)
smart2 <- unite(smart2, Campaign, 1:3, sep = "-")

# brandnaam toevoegen aan Brand
brandnaam2 <- sub(" -.*", "", gsub(pattern = "Januari|Februari|Maart|April|Mei|Juni|Juli|Augustus|September|Oktober|November|December", 
                                  x = basename(vec_ruw_doel), "-", ignore.case = T))
smart2$Brand <- brandnaam2
