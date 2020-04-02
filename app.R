
#™Chris Dide-Agossou
############################################################################################################################################                 
############################################################################################################################################                 

## load data
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
library(V8)
library(jsonlite)
library(shinyWidgets)
library(shinyMobile)
#library(shinydashboard)
#library(shinydashboardPlus)



######################################################################################################################################################################################                
######################################################################################################################################################################################                

## load data

##### FIRST DATA SOURCE
CASES <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"), header=T)
names(CASES)[2] <- "Country"
CASES$Country <- as.character(CASES$Country)
CASES$Country[CASES$Country %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
CASES$Country[CASES$Country %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"


DEATHS <-read.csv(file=("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"), header=T)
names(DEATHS)[2] <- "Country"
DEATHS$Country <- as.character(DEATHS$Country)
DEATHS$Country[DEATHS$Country %in% "Congo (Brazzaville)"] <- "Republic of the Congo"
DEATHS$Country[DEATHS$Country %in% "Congo (Kinshasa)"] <- "Democratic Republic of the Congo"

######################################################################################################################################################################################                
######################################################################################################################################################################################                

##### SECOND DATA SOURCE
cv_cases <-read.csv(file=("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/coronavirus.csv"), header=T)

countries <- read.csv(file=("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/countries_codes_and_coordinates.csv"), header=T)

worldcountry <- read.csv(file=("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/country_geoms.csv"), header=T)


######################################################################################################################################################################################                
######################################################################################################################################################################################                

## import translation bin
load("../AFRICA_COVID19_visualization/translation and dictionary/translation.bin") 



############################
## LIST of AFRICAN COUNTRIES
############################
COUNTRY <-as.data.frame(cbind(seq(1:54) ,c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cameroon", "Cabo Verde", "Central African Republic", "Chad", "Camoros",
                                           "Democratic Republic of the Congo", "Republic of the Congo", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Ethiopia", "Gabon", "Gambia", 
                                           "Ghana", "Guinea", "Guinea-Bissau", "Cote d'Ivoire", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", 
                                           "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", 'Rwanda', "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", 
                                           "Somalia", "South Africa", "South Sudan", "Sudan", "Swaziland", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")))

names(COUNTRY)  <- c("rank", "Country")


### merge list of the countries
CASES2 <- inner_join(CASES, COUNTRY)
CASES2 <- CASES2[, -ncol(CASES2)]

DEATHS2 <- inner_join(DEATHS, COUNTRY)
DEATHS2 <- DEATHS2[, -ncol(DEATHS2)]


## Define otal cases and deaths for each country
CASES3 <- CASES2[, c(2:4, ncol(CASES2))]
DEATHS3 <- DEATHS2[, c(2:4, ncol(DEATHS2))]

CASES_DEATHS <- merge(CASES3, DEATHS3, by = c("Country", "Lat", "Long"))
names(CASES_DEATHS) <- c("Country", "Lat", "Long", "Cases", "Deaths")

##
CASES_DEATHS$Pays <- CASES_DEATHS$Country
CASES_DEATHS$Cas <- CASES_DEATHS$Cases
CASES_DEATHS$Décès <- CASES_DEATHS$Deaths




### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death10)), "../AFRICA_COVID19_visualization/clean_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% 
  select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% 
  group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()


# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}
write.csv(cv_cases_continent, "../AFRICA_COVID19_visualization/clean_data/coronavirus_continent.csv")




############################################################################################################################################                 
############################################################################################################################################                 

# rsconnect::setAccountInfo(name='dideagoc',
#                           token='BB5160165A58CDC671D9AEC6A129FB71',
#                           secret='5+ZxydNAWONtTILrbipc2eNqar7aMwTfZc/NRaDh')
# 
# rsconnect::deployApp("../WHO_burden_and_mortality_visualization/")

############################################################################################################################################                 
############################################################################################################################################                 

# ##get your token to access google drive
# shiny_token2 <- gs_auth(new_user = TRUE)
# saveRDS(shiny_token2, "shiny_app_token.rds")
# #gs_auth(token = "../AFRICA_COVID19_visualization/shiny_app_token.rds") # from .rds file


#set up data sheet in google drive
 # Data <- gs_new("Data") %>%
 #   gs_ws_rename(from = "Sheet1", to = "Data")
 # Data <- Data %>%
 #   gs_edit_cells(ws = "Data", input = cbind("name", "sender.email", "my.email", "msg", "timestamp"), trim = TRUE)


 ## Note: for some reason it wont work if first row is blank so I went into the google sheet
## and put in some values in the first few rows

# sheetkey <-  "1rxVKig8hjtOElWeGWunK5c96Oh-aIxpVjuIwf_D6UJM"
# # Data$sheet_key # you can get your key from Data$sheet_key, don't share your sheet key!
# Data <- gs_key(sheetkey)

############################################################################################################################################                 
############################################################################################################################################                 



ui2 <- 

############################################################################################################################################                 
### 1ST TAB-PANEL

  navbarPage(inverse=T,
            "COVID-19",
             tabPanel("AFRICA",
              fluidPage(HTML('<meta name="viewport" content="width=1024">'),
               fluidRow(
                 # tags$head(
                 #          tags$style(".leaflet-container { background: #ddd; }")
                 #        ),
                 column(4, 
                  "",
              
                  radioButtons(inputId = "language1", 
                               label = (""),
                               choices = (c("English" = "en", "Français" = "fr")),
                               inline = TRUE,
                               selected = ("en")),
                  
             
                  h4(textOutput("reactive_case_count"), align = "right"),
                 
                   h4(textOutput("reactive_death_count"), align = "right"),
                  
                  dataTableOutput("Africa")
                  
                  ),
                 
                  column(4,
                  "",
                  uiOutput("uiburden1"),
                  
                  #box(leafletOutput("map", height=675, width = 500))
                  leafletOutput("map", height=600, width = 600)
                  )
                 ,
                  
                  
                  column(4, offset = 0,
                         "",
                         fluidRow( 
                           plotOutput("cumulative", height = "100%"),
                           plotOutput("consecutive", height = "100%"))
                  )
                 )
               )
                    #  )
             ),
             
############################################################################################################################################                 
### 2ND TAB-PANEL
             tabPanel(
               "Information", 
               
               radioButtons(inputId = "language8", label = "",
                            choices = c("English" = "en", "Français" = "fr"),
                            inline = TRUE,
                            selected = "en"),
               fluidRow(
                 column(6,
                        uiOutput("uicmt81"),
                        hr(),
                        
                        uiOutput("uicmt82"),
                        
                        a("Johns Hopkins Center for Systems Science and Engineering", 
                          href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
                        
                        hr(),
                        
                        uiOutput("uicmt83"),
                        
                        a("(1) https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization"),
                        
                        br(),
                        
                        a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
                        
                                      
                        hr(),
                       
                         a(strong("Code"), href="https://github.com/ddkunda/WHO-data-visualization"),
                        
                        br(),
                        hr(),
                        
                        uiOutput("uicmt84"),
                        
                        uiOutput("uicmt85"),
                        
                        a("christian.dide-agossou@cuanschutz.edu",
                          br(),
                          href="mailto:christian.dide-agossou@cuanschutz.edu")
                        
                 ),
                 
                 
                 column(4, offset = 1,
                        uiOutput("uicmt86"),
                        uiOutput("uicmt87"),
                        uiOutput("uicmt88"),
                        uiOutput("uimsg81"),
                        uiOutput("uicmt89")
                        
                 )
               )
             )
             
############################################################################################################################################                 
 
 )

############################################################################################################################################                 

  




server2 <- function(input, output){
  

############################################################################################################################################                 
### 1ST TAB-PLOT 
  
  # Translation for TAB1
  tr1 <- function(text){ # translates text into current language
    sapply(text,function(s) translation[[s]][[input$language1]], USE.NAMES=FALSE)
  }
  
  
  output$uiburden1 <- renderUI({
    radioButtons(inputId =  "burden1",
                label = tr1("Choose a burden measure:"),
                choices = tr1(c("Cases", "Deaths")),
                inline = TRUE,
                selected  = tr1("Cases"))
  })
  

  reactive_df = reactive({
    cv_cases_continent[cv_cases_continent$continent_level %in% "Africa", ]
  })
  
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_df()$new_cases), big.mark=","), tr1(" total cases"))
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_df()$new_deaths), big.mark=","), tr1(" total deaths"))
  })
  
  
  output$Africa <- renderDataTable({
    
    df <- as.data.frame(CASES_DEATHS[order(CASES_DEATHS$Cases, decreasing = TRUE), ])
    
    rownames(df) <- c()
    
    df[, c(tr1("Country"), tr1("Cases"), tr1("Deaths"))]
    })
  
  
  output$map <- renderLeaflet({
    
    
    req(input$burden1)
    req(CASES_DEATHS)
    
    
    df1 <- CASES_DEATHS
    
    if(input$burden1 %in% paste(tr1("Cases"))){
      leaflet(df1)  %>%
        setView(lng = 25, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*0.03,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="red",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.DarkMatter)
      }
    
#  Stamen.TonerLite 
# CartoDB.DarkMatter
# CartoDB.Positron    
    
    else if(input$burden1 %in% paste(tr1("Deaths"))){
      leaflet(df1)  %>%
        setView(lng = 25, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*0.2,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="red",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.DarkMatter)
                         }
    })
  
  
  output$cumulative <- renderPlot({
    
    req(cv_cases_continent)
  df2 <- cv_cases_continent[cv_cases_continent$continent_level %in% "Africa", ]
  
  ##Subset the necessary columns
  df2 = df2[,c("date", "cases", "deaths")]
  df2b = melt(df2, id=c("date"))
  
  
  ggplot(df2b)+ 
    geom_line(aes(x=date, y=value, colour=variable), size=1.5)+
    scale_colour_manual(values=c("blue","red"))+
    ylab("count")+
    theme_bw()+
    theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
    theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=12))+
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
    theme(axis.text.x = element_text(size=10, color = "black", face="bold"),
          axis.text.y = element_text(size=10, color = "black", face="bold"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"))
  # +
  #   theme(plot.margin = margin(19,50, 50,19))

    }
   , height = 300, width = 450
)
  
output$consecutive <- renderPlot({
    
    req(cv_cases_continent)
    df3 <- cv_cases_continent[cv_cases_continent$continent_level %in% "Africa", ]
    
    ggplot(df3,  aes(x = date, y = new_cases)) + 
      geom_bar(position="stack", stat="identity", col="black", fill="orange") + 
      ylab("new cases") + theme_bw() + 
      geom_text(aes(label=new_cases), size=2.5, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
      scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
      theme_bw()+
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
      theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=12))+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      theme(axis.text.x = element_text(size=10, color = "black", face="bold"),
            axis.text.y = element_text(size=10, color = "black", face="bold"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    # +
    #   theme(plot.margin = margin(19,50, 50,19))
    
  }
  , height = 365, width = 450
  )
  
 



############################################################################################################################################                 
### 2ND TAB-PLOT 
  
  ## Translation for TAB7
  tr2 <- function(text){
    sapply(text,function(s) translation[[s]][[input$language8]], USE.NAMES=FALSE)
  }
  
  # UI for TAB8
  output$uicmt81 <- renderUI({
    helpText(h4(em(tr2("This Shiny App provides a quick way to search COVID-19 data in AFRICA, including the number of cases and deaths at the country level."))))
  })
  
  output$uicmt82 <- renderUI({
    helpText(tr2("Adapted from timeline data published by: "))
  })
  
  output$uicmt83 <- renderUI({
    helpText(tr2("Code adapted from the following sources:"))
  })
  
  
  output$uicmt84 <- renderUI({
    helpText(tr2("Any questions or comments can be sent to:"))
  })
  
  
  output$uicmt85 <- renderUI({
    helpText(strong(tr2("Christian Dide-Agossou, MS, BS. (PhD Candidate)")))
  })
  
  output$uicmt86 <- renderUI({
    textInput("name", 
              tr2("Name:"),
              value="")
  })
  
  output$uicmt87 <- renderUI({
    textInput("from", 
              tr2("From:"), 
              value="xxx@gmail.com")
  })
  
  output$uicmt88 <- renderUI({
    textInput("to", 
              tr2("To:"), 
              value="christian.dide-agossou@cuanschutz.edu")
  })
  
  output$uimsg81 <- renderUI({
    textAreaInput("mgs", tr2("Message"), value = "", width = '100%', rows = 5, resize = "both")
  })
  
  
  output$uicmt89 <- renderUI({
    actionButton("send", 
                 tr2("Submit"))
  })
  
  
  ## MAINPANEL for TAB8
  #store the results
  Results <- reactive(c(
    input$name, input$from, input$to, input$mgs, Sys.time()
  ))
  
  #This will add the new row at the bottom of the dataset in Google Sheets.
  observeEvent(input$send, {                                                                 
    Data  <- Data  %>%                                                                      
      gs_add_row(ws = "Data", input = Results())                                                               
  })
  
  
############################################################################################################################################                 
  
}

############################################################################################################################################                 

shinyApp(ui2, server2)
