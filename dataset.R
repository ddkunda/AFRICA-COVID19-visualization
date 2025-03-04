stringsAsFactors = FALSE
#™Chris Dide-Agossou
############################################################################################################################################                 
############################################################################################################################################                 

## load data
library(shiny)
library(tidyverse)
library(rsconnect)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
library(XML)


######################################################################################################################################################################################                
######################################################################################################################################################################################                
##### TASK I
######################################################################################################################################################################################                
######################################################################################################################################################################################                


############################
## LIST of AFRICAN COUNTRIES
############################
# AFRICA <-as.data.frame(cbind(seq(1:54),c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Camoros",
#                                           "Democratic Republic of the Congo", "Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia",
#                                           "Ghana", "Guinea", "Guinea-Bissau", "Côte d'Ivoire", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania",
#                                           "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 'Rwanda', "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone",
#                                           "Somalia", "South Africa", "South Sudan", "Sudan", "Eswatini", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")))
# 
# names(AFRICA) <- c("Rank", "Country")

AFRICA <-c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cape Verde", "Central African Republic", "Chad", "Comoros",
                                         "Democratic Republic of the Congo", "Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia",
                                         "Ghana", "Guinea", "Guinea-Bissau", "Côte d'Ivoire", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania",
                                         "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 'Rwanda', "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone",
                                         "Somalia", "South Africa", "South Sudan", "Sudan", "Eswatini", "Tanzania", "Togo", "Tunisia", "Uganda", "Western Sahara","Zambia", "Zimbabwe")


### import Africa population data
Africa_countries <- read.csv("population.csv", header = TRUE)
Africa_countries$Pop. <- gsub(",", "", Africa_countries$Pop.)
Africa_countries$Pop. <- as.numeric(Africa_countries$Pop.)    

Africa_countries$Country <- as.character(Africa_countries$Country)
Africa_countries$Country[Africa_countries$Country %in% "C\x99te d'Ivoire"] <- "Côte d'Ivoire"
Africa_countries <- Africa_countries[Africa_countries$Country %in% AFRICA, ]

######################################################################################################################################################################################                
######################################################################################################################################################################################                

## import translation files
load("translation.bin") 
translationContent <- read.csv("dictionary.csv", header = TRUE)

######################################################################################################################################################################################                
######################################################################################################################################################################################                

##### FIRST DATA SOURCE
CASES_long <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=T)
names(CASES_long)[2] <- "Country"
CASES_long$Country <- as.character(CASES_long$Country)
CASES_long$Country[CASES_long$Country %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
CASES_long$Country[CASES_long$Country %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
CASES_long$Country[CASES_long$Country %in% "Cote d'Ivoire"] <- "Côte d'Ivoire"
CASES_long$Country[CASES_long$Country %in% "Cabo Verde"] <- "Cape Verde"


CASES_long2 <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=F)
CASES_long2$V2 <- as.character(CASES_long2$V2)
CASES_long2$V2[CASES_long2$V2 %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
CASES_long2$V2[CASES_long2$V2 %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
CASES_long2$V2[CASES_long2$V2 %in% "Cote d'Ivoire"] <- "Côte d'Ivoire"
CASES_long2$V2[CASES_long2$V2 %in% "Cabo Verde"] <- "Cape Verde"
CASES_long2 <- CASES_long2[CASES_long2$V2 %in% c("Country/Region", AFRICA), ]


DEATHS_long <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), header=T)
names(DEATHS_long)[2] <- "Country"
DEATHS_long$Country <- as.character(DEATHS_long$Country)
DEATHS_long$Country[DEATHS_long$Country %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
DEATHS_long$Country[DEATHS_long$Country %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
DEATHS_long$Country[DEATHS_long$Country %in% "Cote d'Ivoire"] <- "Côte d'Ivoire"
DEATHS_long$Country[DEATHS_long$Country %in% "Cabo Verde"] <- "Cape Verde"


DEATHS_long2 <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), header=F)
DEATHS_long2$V2 <- as.character(DEATHS_long2$V2)
DEATHS_long2$V2[DEATHS_long2$V2 %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
DEATHS_long2$V2[DEATHS_long2$V2 %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
DEATHS_long2$V2[DEATHS_long2$V2 %in% "Cote d'Ivoire"] <- "Côte d'Ivoire"
DEATHS_long2$V2[DEATHS_long2$V2 %in% "Cabo Verde"] <- "Cape Verde"
DEATHS_long2 <- DEATHS_long2[DEATHS_long2$V2 %in% c("Country/Region", AFRICA), ]


RECOVERED_long <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"), header=T)
names(RECOVERED_long)[2] <- "Country"
RECOVERED_long$Country <- as.character(RECOVERED_long$Country)
RECOVERED_long$Country[RECOVERED_long$Country %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
RECOVERED_long$Country[RECOVERED_long$Country %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"
RECOVERED_long$Country[RECOVERED_long$Country %in% "Cote d'Ivoire"] <- "Côte d'Ivoire"
RECOVERED_long$Country[RECOVERED_long$Country %in% "Cabo Verde"] <- "Cape Verde"

######################################################################################################################################################################################                
######################################################################################################################################################################################                

##### SECOND DATA SOURCE
if(require(RCurl) && url.exists("https://www.worldometers.info/coronavirus/")) {
  tt =  getURL("https://www.worldometers.info/coronavirus/",  userpwd = "bob:duncantl")
  World <- readHTMLTable(tt)
}

Global_data <- as.data.frame(World[1])

Global_data <- Global_data[,c("main_table_countries_today..","main_table_countries_today.Country.Other",
                              "main_table_countries_today.TotalCases","main_table_countries_today.NewCases",
                              "main_table_countries_today.TotalDeaths","main_table_countries_today.NewDeaths",
                              "main_table_countries_today.TotalRecovered","main_table_countries_today.NewRecovered",
                              "main_table_countries_today.ActiveCases","main_table_countries_today.Serious.Critical",
                              "main_table_countries_today.Tot.Cases.1M.pop","main_table_countries_today.Deaths.1M.pop",
                              "main_table_countries_today.TotalTests","main_table_countries_today.Tests.1M.pop",
                              "main_table_countries_today.Population","main_table_countries_today.Continent" )]

names(Global_data) <- c("Country_num", "Country", 
                        "Total.Cases", "NewCases", 
                        "Total.Deaths", "NewDeaths", 
                        "Total.Recovered","NewRecovered",
                        "ActiveCases", "SeriousCritical", 
                        "Total.Cases.1M.pop", "Deaths.1M.pop",
                        "TotalTests", "Tests.1M.pop",
                        "Population", "Continent") 


Global_data$Country <- as.character(Global_data$Country)
Global_data$Country[Global_data$Country %in% "Cabo Verde"] <- "Cape Verde"
Global_data$Country[Global_data$Country %in% "Congo"] <- "Republic of the Congo"
Global_data$Country[Global_data$Country %in% "DRC"] <-  "Democratic Republic of the Congo"
Global_data$Country[Global_data$Country %in% "Ivory Coast"] <-  "Côte d'Ivoire" 
Global_data$Country[Global_data$Country %in% "CAR"] <-  "Central African Republic"

######################################################################################################################################################################################                
######################################################################################################################################################################################                




######################################################################################################################################################################################                
######################################################################################################################################################################################                
##### TASK I
######################################################################################################################################################################################                
######################################################################################################################################################################################                

## create a dataset for African countries including population size and longitude and latidude
lat_long <- CASES_long[CASES_long$Country %in% AFRICA, ]
lat_long <- lat_long[, c(2:4)]

Africa_countries <- merge(Africa_countries, lat_long, by="Country", all = TRUE)

## input missing long and lat
Africa_countries$Lat[Africa_countries$Country %in% "Western Sahara"] <- 24.22
Africa_countries$Long[Africa_countries$Country %in% "Western Sahara"] <- 12.89

# Africa_countries$Lat[Africa_countries$Country %in% "Comoros"] <- -12.1667	
# Africa_countries$Long[Africa_countries$Country %in% "Comoros"] <- 44.25
# 
# Africa_countries$Lat[Africa_countries$Country %in% "Lesotho"] <- -29.5
# Africa_countries$Long[Africa_countries$Country %in% "Lesotho"] <- 28.5
# 
# Africa_countries$Lat[Africa_countries$Country %in% "Sao Tome and Principe"] <- 1
# Africa_countries$Long[Africa_countries$Country %in% "Sao Tome and Principe"] <- 7
# 
# Africa_countries$Lat[Africa_countries$Country %in% "Mauritania"] <- 20.00
# Africa_countries$Long[Africa_countries$Country %in% "Mauritania"] <- -12.00
# 
# Africa_countries$Lat[Africa_countries$Country %in% "Republic of the Congo"] <- -0.228021
# Africa_countries$Long[Africa_countries$Country %in% "Republic of the Congo"] <- 15.8276587

######################################################################################################################                

## SET1 summary data of coronavirus from Source 1
CASES <- CASES_long[CASES_long$Country %in% AFRICA, ]
CASES <- CASES[, c(2, ncol(CASES))]
names(CASES) <- c("Country", "Cases")

DEATHS <- DEATHS_long[DEATHS_long$Country %in% AFRICA, ]
DEATHS <- DEATHS[, c(2, ncol(DEATHS))]
names(DEATHS) <- c("Country", "Deaths")

RECOVERED <- RECOVERED_long[RECOVERED_long$Country %in% AFRICA, ]
RECOVERED <- RECOVERED[, c(2, ncol(RECOVERED))]
names(RECOVERED) <- c("Country", "Recovered")

set1 <- merge(CASES, DEATHS)
set1 <- merge(set1, RECOVERED)

######################################################################################################################                

## SET2 summary data of coronavirus from Source 2
set2 <- Global_data[Global_data$Country %in% AFRICA, c("Country" ,"Total.Cases","Total.Deaths", "Total.Recovered")]

######################################################################################################################                
### merge set1 and set2 
set1_2 <- merge(set1, set2, by="Country", all = TRUE)

## update columns
set1_2$Total.Cases <- as.character(set1_2$Total.Cases)
set1_2$Total.Cases <- gsub(",", "", set1_2$Total.Cases)
set1_2$Total.Cases <- as.integer(set1_2$Total.Cases)    

set1_2$Total.Deaths <- as.character(set1_2$Total.Deaths)
set1_2$Total.Deaths <- gsub(",", "", set1_2$Total.Deaths)
set1_2$Total.Deaths <- as.integer(set1_2$Total.Deaths)    

set1_2$Total.Recovered <- as.character(set1_2$Total.Recovered)
set1_2$Total.Recovered <- gsub(",", "", set1_2$Total.Recovered)
set1_2$Total.Recovered <- as.integer(set1_2$Total.Recovered)    

## replace missing by zero
set1_2 <- as.matrix(set1_2)


#set1_2[set1_2 %in% NA] <- 0
set1_2 <- as.data.frame(set1_2)

######################################################################################################################################################################################                
######################################################################################################################################################################################                

## create a complete data and group countries by region
CASES_DEATHS_RECOVERED <- merge(Africa_countries, set1_2, by = c("Country"), all = TRUE)

## group countries by region
CASES_DEATHS_RECOVERED$Region <- NA

CASES_DEATHS_RECOVERED$Region[CASES_DEATHS_RECOVERED$Country %in% c("Burundi", "Cameroon","Central African Republic", "Chad","Democratic Republic of the Congo", "Republic of the Congo","Equatorial Guinea","Gabon", "Sao Tome and Principe")] <- "Central"
CASES_DEATHS_RECOVERED$Region[CASES_DEATHS_RECOVERED$Country %in% c("Comoros","Djibouti","Eritrea", "Ethiopia", "Kenya","Madagascar", "Mauritius","Rwanda", "Seychelles", "Somalia","Sudan", "South Sudan","Tanzania","Uganda")] <- "Eastern"
CASES_DEATHS_RECOVERED$Region[CASES_DEATHS_RECOVERED$Country %in% c("Algeria", "Egypt","Libya","Mauritania", "Morocco","Tunisia")] <- "Northern"
CASES_DEATHS_RECOVERED$Region[CASES_DEATHS_RECOVERED$Country %in% c("Angola","Botswana","Eswatini","Lesotho", "Malawi","Mozambique", "Namibia", "South Africa","Zambia", "Zimbabwe")] <- "Southern"
CASES_DEATHS_RECOVERED$Region[CASES_DEATHS_RECOVERED$Country %in% c("Benin", "Burkina Faso","Cape Verde", "Côte d'Ivoire", "Gambia", "Ghana","Guinea", "Guinea-Bissau","Liberia","Mali","Niger", "Nigeria","Senegal", "Sierra Leone", "Togo", "Western Sahara")] <- "Western"


## save
write.csv(CASES_DEATHS_RECOVERED, "CASES_DEATHS_v1.csv", row.names = FALSE)


######################################################################################################################################################################################                
######################################################################################################################################################################################                


## merge with Countries french names
CASES_DEATHS_RECOVERED3 <- merge(CASES_DEATHS_RECOVERED, translationContent, by.x = "Country", by.y = "key", all.x = TRUE)

##convert variables from factors to numeric
for (j in c(7,8,9,10,11,12)) {
  CASES_DEATHS_RECOVERED3[,j] <- as.character(CASES_DEATHS_RECOVERED3[,j])
  CASES_DEATHS_RECOVERED3[,j] <- as.numeric(CASES_DEATHS_RECOVERED3[,j])
}


## create french translation of variables
CASES_DEATHS_RECOVERED3$fr <- as.character(CASES_DEATHS_RECOVERED3$fr)
CASES_DEATHS_RECOVERED3$Pays <- CASES_DEATHS_RECOVERED3$fr
CASES_DEATHS_RECOVERED3$Total.Cas <- CASES_DEATHS_RECOVERED3$Total.Cases
CASES_DEATHS_RECOVERED3$Total.Décès <- CASES_DEATHS_RECOVERED3$Total.Deaths
CASES_DEATHS_RECOVERED3$Total.Rétablis <- CASES_DEATHS_RECOVERED3$Total.Recovered

CASES_DEATHS_RECOVERED3$Region <- as.character(CASES_DEATHS_RECOVERED3$Region)
CASES_DEATHS_RECOVERED3$Région <- CASES_DEATHS_RECOVERED3$Region
CASES_DEATHS_RECOVERED3$Région[CASES_DEATHS_RECOVERED3$Région %in% "Northern"] <- "Nord"
CASES_DEATHS_RECOVERED3$Région[CASES_DEATHS_RECOVERED3$Région %in% "Southern"] <- "Sud"
CASES_DEATHS_RECOVERED3$Région[CASES_DEATHS_RECOVERED3$Région %in% "Eastern"] <- "Est"
CASES_DEATHS_RECOVERED3$Région[CASES_DEATHS_RECOVERED3$Région %in% "Western"] <- "Ouest"
CASES_DEATHS_RECOVERED3$Région[CASES_DEATHS_RECOVERED3$Région %in% "Central"] <- "Centre"

CASES_DEATHS_RECOVERED3$Pays[CASES_DEATHS_RECOVERED3$Country %in% "Cabo Verde"] <- "Cap-Vert"
CASES_DEATHS_RECOVERED3$Pays[CASES_DEATHS_RECOVERED3$Country %in% "Republic of the Congo"] <- "Congo"
CASES_DEATHS_RECOVERED3$Pays[CASES_DEATHS_RECOVERED3$Country %in% "Tanzania"] <- "République-Unie de Tanzanie"
  

## calculate case and mortality rates
CASES_DEATHS_RECOVERED3$Case.Rate <- round(((CASES_DEATHS_RECOVERED3$Total.Cases/CASES_DEATHS_RECOVERED3$Pop.)*1000000),2)
CASES_DEATHS_RECOVERED3$Death.Rate <- round(((CASES_DEATHS_RECOVERED3$Total.Deaths/CASES_DEATHS_RECOVERED3$Pop.)*1000000),2)

CASES_DEATHS_RECOVERED3$Taux.Cas <- CASES_DEATHS_RECOVERED3$Case.Rate
CASES_DEATHS_RECOVERED3$Taux.Décès <- CASES_DEATHS_RECOVERED3$Death.Rate

save(CASES_DEATHS_RECOVERED3, file = "CASES_DEATHS_v3.RData")

######################################################################################################################################################################################                
######################################################################################################################################################################################                

## Cases DATA PROCESSING for longitudinal plot
CASES_long2_lf <- as.data.frame(gather(CASES_long2, "date", "cases", -c(V1, V2, V3,V4)))

#### create sbset1
## 1a
CASES_long2_lf1a <- as.data.frame(cbind(seq(1,ncol(CASES_long2[, (5:ncol(CASES_long2))]),1), c(names(CASES_long2[, (5:ncol(CASES_long2))]))))
names(CASES_long2_lf1a) <- c("ID", "date")
CASES_long2_lf1a$ID <- as.character(CASES_long2_lf1a$ID)
CASES_long2_lf1a$ID <- as.numeric(CASES_long2_lf1a$ID)

## 1b
CASES_long2_lf1b  <- CASES_long2_lf[CASES_long2_lf$V1 %in% "Province/State", ]
CASES_long2_lf1b <- CASES_long2_lf1b[, c("date", "cases")]

## merge 1a and 1b
CASES_long2_lf1 <- merge(CASES_long2_lf1a, CASES_long2_lf1b, by="date")
names(CASES_long2_lf1)[3] <- "real_date"
rownames(CASES_long2_lf1) <- NULL

#### create subset2
CASES_long2_lf2  <- CASES_long2_lf[!CASES_long2_lf$V1 %in% "Province/State", ]
CASES_long2_lf2 <- CASES_long2_lf2[,-1]
names(CASES_long2_lf2) <- c("Country", "Lat","Long", "date", "cases")
CASES_long2_lf2$cases <- as.numeric(CASES_long2_lf2$cases)

#### merge sbset1 and subset2 and order
CASES_long2_lf3 <- merge(CASES_long2_lf1, CASES_long2_lf2, by="date", all = TRUE)
CASES_long2_lf3$real_date <- as.Date(CASES_long2_lf3$real_date, "%m/%d/%y")
CASES_long2_lf3 <- CASES_long2_lf3[order(CASES_long2_lf3$Country, CASES_long2_lf3$real_date), ]

#### estimate the number of daily new cases by country
CASES_long2_lf3$daily_new_cases <- ave(CASES_long2_lf3$cases, factor(CASES_long2_lf3$Country), FUN=function(x) c(NA,diff(x)))
CASES_long2_lf3$daily_new_cases[CASES_long2_lf3$daily_new_cases %in% NA] <- 0
CASES_long2_lf3$daily_new_cases[CASES_long2_lf3$daily_new_cases < 0] <- 0

## tally the daily new cases per date
sum_CASES_long2_lf3a <- aggregate(daily_new_cases ~ real_date, CASES_long2_lf3, sum)
sum_CASES_long2_lf3b <- aggregate(cases ~ real_date, CASES_long2_lf3, sum)

######################################################################################################################################################################################                
######################################################################################################################################################################################                

## DEATHS DATA PROCESSING for longitudinal plot
DEATHS_long2_lf <- as.data.frame(gather(DEATHS_long2, "date", "deaths", -c(V1, V2, V3,V4)))

#### create sbset1
## 1a
DEATHS_long2_lf1a <- as.data.frame(cbind(seq(1,ncol(DEATHS_long2[, (5:ncol(DEATHS_long2))]),1), c(names(DEATHS_long2[, (5:ncol(DEATHS_long2))]))))
names(DEATHS_long2_lf1a) <- c("ID", "date")
DEATHS_long2_lf1a$ID <- as.character(DEATHS_long2_lf1a$ID)
DEATHS_long2_lf1a$ID <- as.numeric(DEATHS_long2_lf1a$ID)

## 1b
DEATHS_long2_lf1b  <- DEATHS_long2_lf[DEATHS_long2_lf$V1 %in% "Province/State", ]
DEATHS_long2_lf1b <- DEATHS_long2_lf1b[, c("date", "deaths")]

## merge 1a and 1b
DEATHS_long2_lf1 <- merge(DEATHS_long2_lf1a, DEATHS_long2_lf1b, by="date")
names(DEATHS_long2_lf1)[3] <- "real_date"
rownames(DEATHS_long2_lf1) <- NULL

#### create subset2
DEATHS_long2_lf2  <- DEATHS_long2_lf[!DEATHS_long2_lf$V1 %in% "Province/State", ]
DEATHS_long2_lf2 <- DEATHS_long2_lf2[,-1]
names(DEATHS_long2_lf2) <- c("Country", "Lat","Long", "date", "deaths")
DEATHS_long2_lf2$deaths <- as.numeric(DEATHS_long2_lf2$deaths)

#### merge sbset1 and subset2 and order
DEATHS_long2_lf3 <- merge(DEATHS_long2_lf1, DEATHS_long2_lf2, by="date", all = TRUE)
DEATHS_long2_lf3$real_date <- as.Date(DEATHS_long2_lf3$real_date, "%m/%d/%y")
DEATHS_long2_lf3 <- DEATHS_long2_lf3[order(DEATHS_long2_lf3$Country, DEATHS_long2_lf3$real_date), ]

#### estimate the number of daily new DEATHS by country
DEATHS_long2_lf3$daily_new_deaths <- ave(DEATHS_long2_lf3$deaths, factor(DEATHS_long2_lf3$Country), FUN=function(x) c(NA,diff(x)))
DEATHS_long2_lf3$daily_new_deaths[DEATHS_long2_lf3$daily_new_deaths %in% NA] <- 0
DEATHS_long2_lf3$daily_new_deaths[DEATHS_long2_lf3$daily_new_deaths < 0] <- 0

## tally the daily new DEATHS per date
sum_DEATHS_long2_lf3a <- aggregate(daily_new_deaths ~ real_date, DEATHS_long2_lf3, sum)
sum_DEATHS_long2_lf3b <- aggregate(deaths ~ real_date, DEATHS_long2_lf3, sum)


######################################################################################################################################################################################                
######################################################################################################################################################################################                

## merge cases and deaths data
CASES_DEATHS_long2 <- merge(sum_CASES_long2_lf3b, sum_DEATHS_long2_lf3b)

CASES_DEATHS_long2$`total cases` <- CASES_DEATHS_long2$cases
CASES_DEATHS_long2$`total des cas`<- CASES_DEATHS_long2$cases
CASES_DEATHS_long2$`total deaths` <- CASES_DEATHS_long2$deaths
CASES_DEATHS_long2$`total des décès` <- CASES_DEATHS_long2$deaths
#save(CASES_DEATHS_long2, file="coronavirus_continent.RData")


######################################################################################################################################################################################                
######################################################################################################################################################################################                

