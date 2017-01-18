---
title: "Handleiding Starcompakket in R"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
  
#### Installatie  
1. Ga naar [`S:\Insights\5 - Business & Data Solutions\14. R\Installatie R`](S:\Insights\5 - Business & Data Solutions\14. R\Installatie R)
2. Kopieer de map Starcom naar: `Documenten\R\R-3.3.2\library\`
3. Na het opstarten van R (kies altijd R i386 en niet de x64 versie) laad je het Starcom pakket in door `library(Starcom)` in het
console in te typen.    

#### Gebruik  
Als je het Starcom pakket hebt in geladen kun je de volgende functies gebruiken:
<br>
```{r eval=FALSE}
update_dma()
```
Na het intypen van deze functie kun je selecteren welke bestanden je wilt laten bewerken. Je kunt meerder bestanden tegelijk selecteren. Wanneer de bewerkingen klaar zijn wordt je gevraagd een opslagmap te kiezen waarna de bestanden daarheen worden geëxporteerd in .xlsx formaat.  
<br>

```{r eval=FALSE}
update_smartcontent(platform)
```
Deze functie vereist dat je tussen de haakjes invoert van welk platform de data afkomstig is. Dit dien je in te typen tussen aanhalingstekens bijv:  
`update_smartcontent(“Facebook”)`  
Je kunt meerdere bestanden tegelijk selecteren.  Wanneer de bewerkingen klaar zijn worden de outputbestanden geëxporteerd naar de map die direct boven de map ligt waar de inputbestanden in staan.  
Dus als de inputbestanden bijvoorbeeld komen uit:  
`\SmartContent\Rapportages 2016\November\Ruw`  
dan worden de outputbestanden opgeslagen in:  
`\SmartContent\Rapportages 2016\November`  
<br>
```{r eval=FALSE}
update_urendashboard()
```
Na het intypen van deze functie wordt je eerst gevraagd het meest recente SC-uren bestand te selecteren en daarna het meest recente VX-uren bestand. Wanneer de bewerkingen klaar zijn wordt de output geëxporteerd naar `Bronbestand Urendashboard werkversie.xlsx`  
<br>
```{r eval=FALSE}
update_zenith()
```
Na het intypen van deze functie wordt je gevraagd het meest recente ZO-uren bestand te selecteren. Wanneer de bewerkingen klaar zijn wordt de output geëxporteerd naar ….?


