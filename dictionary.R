library(plyr)
library(shiny)
library(tidyverse)
library(png)
library(grid)
library(gridExtra)
library(rsconnect)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(reshape2)
library(knitr)
library(sendmailR)
library(shinyAce)
library(mailR)
library(googlesheets)
library(DT)
library(RCurl)
library(curl)
library(httr)
#library(shinydashboard)
#library(shinydashboardPlus)


############################################################################################################################################                 
############################################################################################################################################                 

## import translation bin
translationContent <- read.delim("../AFRICA_COVID19_visualization/dictionary.csv", header = TRUE, sep = ",", as.is = TRUE) 
translation <- dlply(translationContent ,.(key), function(s) key = as.list(s))
save(translation, file = "../AFRICA_COVID19_visualization/translation.bin")
load("../AFRICA_COVID19_visualization/translation.bin") 




