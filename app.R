library(rsconnect)
library(dplyr)
library(ggplot2)
library(maps)
library(mapproj)
library(tidyr)
library(formattable)
library(tidyverse)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
library(XML)
library(RCurl)
library(rvest)
library(scales)
library(shiny)
library(bs4Dash)


zstringsAsFactors = FALSE


#™Chris Dide-Agossou
############################################################################################################################################                 
############################################################################################################################################                 

######################################
## Part. 1 load data
######################################

source("dataset.R")
source("dictionary.R")

## load AfricaCDC data
load("CASES_DEATHS_v3.RData")

## import country and DiseaseGroup translation
load("translation.bin") 
country_translation <- read.csv("country_translation.csv", header = TRUE)

### make summary table for region and calculate case and death rates
CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED3[, c("Region","Total.Cases", "Total.Deaths", "Pop.")]
CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED4%>%
  group_by(Region)%>%
  summarise_if(is.numeric, funs(sum), na.rm=TRUE)
   
 


CASES_DEATHS_RECOVERED4 <- as.data.frame(CASES_DEATHS_RECOVERED4)
CASES_DEATHS_RECOVERED4$Case.Rate <- round(((CASES_DEATHS_RECOVERED4$Total.Cases/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)
CASES_DEATHS_RECOVERED4$Death.Rate <- round(((CASES_DEATHS_RECOVERED4$Total.Deaths/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)

## french columns
CASES_DEATHS_RECOVERED4$Région <- CASES_DEATHS_RECOVERED4$Region
CASES_DEATHS_RECOVERED4$Total.Cas <- CASES_DEATHS_RECOVERED4$Total.Cases
CASES_DEATHS_RECOVERED4$Total.Décès <- CASES_DEATHS_RECOVERED4$Total.Deaths
CASES_DEATHS_RECOVERED4$Taux.Cas <- CASES_DEATHS_RECOVERED4$Case.Rate
CASES_DEATHS_RECOVERED4$Taux.Décès <- CASES_DEATHS_RECOVERED4$Death.Rate

### compare the two sources
sum(CASES_DEATHS_RECOVERED3$Cases, na.rm = TRUE)
sum(CASES_DEATHS_RECOVERED3$Total.Cases, na.rm = TRUE)
sum(CASES_DEATHS_RECOVERED3$Deaths, na.rm = TRUE)
sum(CASES_DEATHS_RECOVERED3$Total.Deaths, na.rm = TRUE)

### re-order regions
CASES_DEATHS_RECOVERED4$Region <- factor(CASES_DEATHS_RECOVERED4$Region,
                                         levels = c("Central","Eastern","Northern","Southern","Western"))

CASES_DEATHS_RECOVERED4$Région <- factor(CASES_DEATHS_RECOVERED4$Région,
                                         levels = c("Central","Eastern","Northern","Southern","Western"),
                                         labels = c("Centre","East","Nord","Sud","Ouest"))

############################################################################################################################################                 
############################################################################################################################################                 

######################################
# Part 2. UI Design
######################################

######################################
# A- Header
header <- dashboardHeader(title = "AFRICA COVID-19 Dashboard")

######################################
# B- Sidebar
sidebar <- bs4DashSidebar(width = 1, fixed = TRUE,expandOnHover = FALSE,
                          bs4SidebarMenu(id = "sidebarmenu",
                                         bs4SidebarMenuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              conditionalPanel("input.sidebarmenu === 'dashboard'",
                               selectInput("language1", "Language:",
                                           choices=list("English" = "en", "Français" = "fr"),
                                           #inline = TRUE,
                                           selected = ("en")),
                               uiOutput("uiburden1"),
                               # span(h6(textOutput("uicmt13"), style="color:orange")),
                               # span(h6(textOutput("uicmt14")), style="color:golden")
                               # hr(),
                               # uiOutput("uicmt13"),
                               # hr(),
                               # uiOutput("uicmt14")
                               )
              )
  )

######################################
# C- Content


frow1 <- fluidRow(
  bs4ValueBoxOutput("reactive_case_count")
  ,bs4ValueBoxOutput("reactive_death_count")
  ,bs4ValueBoxOutput("reactive_recovered_count")
)

frow2 <- fluidRow( 
  box(
    title = ""
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,leafletOutput("map"))
  ,
  box(
    title = ""
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("regionbarplot"))

)


frow3 <- fluidRow( 
  box(
    title = ""
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("consecutiveCases")
    )
  ,
  box(
    title = ""
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("consecutiveDeaths")
#    ,splitLayout(cellWidths = c("50%", "50%"), plotOutput("consecutiveCases"), plotOutput("consecutiveDeaths"))
  )
)


frow4 <- box(
  width = 12,
  title = ""
  ,status = "primary"
  ,solidHeader = FALSE
  ,collapsible = FALSE,
  span(h6(textOutput("uicmt13"), style="color:orange")),
  span(h6(textOutput("uicmt14")), style="color:golden"),
  h6(a(strong("Code"), href="https://github.com/ddkunda/AFRICA-COVID19-visualization"))

)


  

######################################
# D- Body

body <- bs4DashBody(
  bs4TabItems(
    bs4TabItem(tabName = "dashboard",
            frow1,frow2,frow3
            ,frow4
    )
  )
)


######################################
# Put everything together
ui <- bs4DashPage(title = 'AFRICA COVID-19 Dashboard', header, sidebar, body, 
                  fullscreen = TRUE,
                  help = FALSE,
                  dark = TRUE,
                  scrollToTop = TRUE)


############################################################################################################################################                 
############################################################################################################################################                 


######################################
# Part 3. Server logic 
######################################

server <- shinyServer(function(input, output) {
  
# Translation
    tr1 <- function(text){ # translates text into current language
      sapply(text, function(s) translation[[s]][[input$language1]], USE.NAMES=FALSE)
    }


    output$uiburden1 <- renderUI({
      radioButtons(inputId =  "burden1",
                  label = "", #tr1("Choose a burden measure:"),
                  choices = tr1(c("Total.Cases", "Total.Deaths", "Case.Rate", "Death.Rate")),
                  inline = FALSE,
                  selected  = tr1("Total.Cases"))
    })


    reactive_df = reactive({
      CASES_DEATHS_RECOVERED3
    })

######################################
## Box1
  output$reactive_case_count <- renderbs4ValueBox({
    case_count_value <-  (sum(reactive_df()$Total.Cases, na.rm = TRUE))
    bs4ValueBox(
      value = tags$p(paste(formatC(case_count_value, format = "d", big.mark = ',')), style = "font-size: 250%;")
      ,subtitle = tr1(" total cases")
      ,color = 'olive'
      ,icon = icon("", lib='font-awesome')
    )
  })
  
#####################################
## Box2
  output$reactive_death_count <- renderbs4ValueBox({
    death_count_value <- (sum(reactive_df()$Total.Deaths, na.rm = TRUE))
    bs4ValueBox(
      value = tags$p(paste(formatC(death_count_value, format = "d", big.mark = ',')), style = "font-size: 250%;")
      ,subtitle = tr1(" total deaths")
      ,color = 'warning'
      ,icon = icon("", lib= 'font-awesome')
    )
  })
  
######################################
## Box3
  output$reactive_recovered_count <- renderbs4ValueBox({
    recovered_count_value <- (sum(reactive_df()$Total.Recovered, na.rm = TRUE))
    bs4ValueBox(
    value = tags$p(paste(formatC(recovered_count_value, format = "d", big.mark = ',')), style = "font-size: 250%;")
      ,subtitle = tr1(" total recovered")
      ,color = 'danger'
      ,icon = icon("", lib= 'font-awesome')
    , elevation = 0
    )
  })  
  
######################################
## Map
  output$map <- renderLeaflet({

      req(input$burden1)
      req(CASES_DEATHS_RECOVERED3)


      df1 <- CASES_DEATHS_RECOVERED3

      if(input$burden1 %in% paste(tr1("Total.Cases"))){
        leaflet(df1)  %>%
          setView(lng = 15, lat = 0, zoom = 3)%>%
          addCircleMarkers(df1,
                           lng = ~Long,
                           lat = ~Lat,
                           radius = df1[, input$burden1]*0.00001,
                           weight = 1,
                           opacity = 4,
                           fill = TRUE,
                           col="green",
                           label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
          addProviderTiles(providers$CartoDB.Positron)
        }

      else if(input$burden1 %in% paste(tr1("Total.Deaths"))){
        leaflet(df1)  %>%
          setView(lng = 15, lat = 0, zoom = 3)%>%
          addCircleMarkers(df1,
                           lng = ~Long,
                           lat = ~Lat,
                           radius = df1[, input$burden1]*0.0001,
                           weight = 1,
                           opacity = 4,
                           fill = TRUE,
                           col="orange",
                           label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
          addProviderTiles(providers$CartoDB.Positron)
                           }
      
      else if(input$burden1 %in% paste(tr1("Case.Rate"))){
      leaflet(df1)  %>%
        setView(lng = 15, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*0.0001,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="red",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1M population"), ")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.Positron)
    }


      else if(input$burden1 %in% paste(tr1("Death.Rate"))){
        leaflet(df1)  %>%
          setView(lng = 15, lat = 0, zoom = 3)%>%
          addCircleMarkers(df1,
                           lng = ~Long,
                           lat = ~Lat,
                           radius = df1[, input$burden1]*0.009,
                           weight = 1,
                           opacity = 4,
                           fill = TRUE,
                           col="blue",
                           label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1M population"),")", ":", df1[, input$burden1]))%>%
          addProviderTiles(providers$CartoDB.Positron)
      }

  })

    
######################################
## Region barplot
    output$regionbarplot <- renderPlot({
      
      
      req(input$burden1)
      req(CASES_DEATHS_RECOVERED4)
      
      
      df2 <- CASES_DEATHS_RECOVERED4
      
      if(input$burden1 %in% paste(("Total.Cases"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Region"))])) +
          geom_col(width = 0.6,fill = "palegreen3")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Total.Cas"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Région"))])) +
          geom_col(width = 0.6,fill = "palegreen3")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Total.Deaths"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Region"))])) +
          geom_col(width = 0.6,fill = "gold1")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Total.Décès"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Région"))])) +
          geom_col(width = 0.6,fill = "gold1")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Case.Rate"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Region"))])) +
          geom_col(width = 0.6,fill = "red")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      
      else if(input$burden1 %in% paste(("Taux.Cas"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Région"))])) +
          geom_col(width = 0.6,fill = "red")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Death.Rate"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Region"))])) +
          geom_col(width = 0.6,fill = "blue")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
      else if(input$burden1 %in% paste(("Taux.Décès"))){
        
        ##Render a barplot
        ggplot(df2,
               aes(x=df2[,input$burden1],
                   y= df2[,c(("Région"))])) +
          geom_col(width = 0.6,fill = "blue")+
          ylab("")+
          scale_x_continuous(labels=comma,expand = c(0, 0))+
          xlab(input$burden1)+
          coord_flip()+
          theme_bw()+
          theme(legend.position = "", axis.title = element_text(family = "", color="black", size=12))+
          theme(axis.text = element_text(size=10, color = "black", angle = 0, hjust = 0.5))+
          ggtitle(tr1("Region"))
      }
      
    })
    
######################################
## Trend plot 1
   
     output$consecutiveCases <- renderPlot({

      req(sum_CASES_long2_lf3a)
      df3 <- sum_CASES_long2_lf3a

      ggplot(df3,  aes(real_date, daily_new_cases))+
        #geom_bar(stat="identity", col="black", fill="blue")+
        geom_density(stat = "identity", col="blue",size=0.1)+
        xlab("")+
        ylab(tr1("daily new cases"))+
        #scale_x_date(date_breaks = "1 month", date_labels = "%b")+
        scale_x_date(date_breaks = "6 month", labels = date_format("%b-%y")) +
        #ylim(0,max(df3$value+10000))+
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        #geom_text(aes(label=paste(" ",daily_new_cases)), size=2, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
        theme_bw()+
        theme(panel.background = element_rect(fill = "grey90", colour = "#6D9EC1"))+
        #theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
        theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=14))+
        theme(axis.text.x = element_text(angle = 30,hjust=1,vjust=1))+
        theme(axis.text.x = element_text(size=8, color = "black", face="bold"),
              axis.text.y = element_text(size=8, color = "black", face="bold"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"))
      #  +
      # theme(plot.margin = margin(15,30, 30,15))

    }
    #, height = 365, width = 450
    #, height = 450, width = 500
    )

######################################
## Trend plot 2
    
    output$consecutiveDeaths <- renderPlot({

      req(sum_DEATHS_long2_lf3a)
      df4 <- sum_DEATHS_long2_lf3a

      ggplot(df4,  aes(real_date, daily_new_deaths))+
        #geom_bar(stat="identity", col="black", fill="red")+
        geom_density(stat = "identity", col="red",size=0.1)+
        xlab("")+
        ylab(tr1("daily new deaths"))+
        #scale_x_date(date_breaks = "1 month", date_labels = "%b")+
        #ylim(0,max(df4$value+200))+
        #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
        scale_x_date(date_breaks = "6 month", labels = date_format("%b-%y")) +
        #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
        #geom_text(aes(label=paste(" ",daily_new_deaths)), size=2, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
        theme_bw()+
        theme(panel.background = element_rect(fill = "grey90", colour = "#6D9EC1"))+
        #theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
        theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=14))+
        theme(axis.text.x = element_text(angle = 30,hjust=1,vjust=1))+
        theme(axis.text.x = element_text(size=8, color = "black", face="bold"),
              axis.text.y = element_text(size=8, color = "black", face="bold"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              axis.line = element_line(colour = "black"))
       # +
       #   theme(plot.margin = margin(15,30, 30,15))

    }
    #, height = 365, width = 450
    #, height = 450, width = 500
    )

    
  
######################################
## Bottom comment
    
output$uicmt13 <- renderText({(tr1("Summary of AFRICA COVID-19 data, including number of cases and deaths by country and region. Case and death rates are per 1,000,000 population."))})
output$uicmt14 <- renderText({(tr1("Any question can be sent to Christian Dide-Agossou, PhD: christian.dideagossou@gmail.com"))})
    
    
    
    # output$uicmt13 <- renderUI({
    #   
    #   helpText("Summary of AFRICA COVID-19 data,",
    #            "including number of cases and deaths by",
    #            "country and region. Case and death rates",
    #            "are per 1,000,000 population.")
    #   })
    #   
    #   # helpText(h6(("Summary of AFRICA COVID-19 data,")),
    #   #          h6(("including number of cases and deaths by")),
    #   #          h6(("country and region. Case and death rates")),
    #   #          h6(("are per 1,000,000 population."))
    #   #          )
    #   # })
    # 
    # output$uicmt14 <- renderUI({
    #   
    #   helpText(h6(("Any question can be sent to Christian Dide-Agossou, PhD:")),
    #            h6(("christian.dideagossou@gmail.com"))
    #   )
    # })
    

  
})

# Run the application 
shinyApp(ui = ui, server = server)




# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ############################################################################################################################################                 


## OLD CODES BELOW THIS LINE

#############################################################################################################################################                 











# #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
# 
# 
# zstringsAsFactors = FALSE
# #™Chris Dide-Agossou
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# ## load data
# library(shiny)
# library(tidyverse)
# library(rsconnect)
# library(reshape2)
# library(leaflet)
# library(leaflet.extras)
# library(DT)
# library(XML)
# library(RCurl)
# library(rvest)
# library(scales)
# 
# ######################################################################################################################################################################################                
# ######################################################################################################################################################################################                
# 
# source("dataset.R")
# source("dictionary.R")
# 
# ## load AfricaCDC data
# load("CASES_DEATHS_v3.RData")
# 
# ## import country and DiseaseGroup translation
# load("translation.bin") 
# country_translation <- read.csv("country_translation.csv", header = TRUE)
# 
# 
# ### make summary table for region and calculate case and death rates
# CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED3[, c("Region","Total.Cases", "Total.Deaths", "Pop.")]
# CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED4%>%
#   group_by(Region)%>%
#   summarise_if(is.numeric, funs(sum), na.rm=TRUE)
# 
# CASES_DEATHS_RECOVERED4 <- as.data.frame(CASES_DEATHS_RECOVERED4)
# CASES_DEATHS_RECOVERED4$Case.Rate <- round(((CASES_DEATHS_RECOVERED4$Total.Cases/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)
# CASES_DEATHS_RECOVERED4$Death.Rate <- round(((CASES_DEATHS_RECOVERED4$Total.Deaths/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)
# 
# 
# ## french columns
# CASES_DEATHS_RECOVERED4$Région <- CASES_DEATHS_RECOVERED4$Region
# CASES_DEATHS_RECOVERED4$Total.Cas <- CASES_DEATHS_RECOVERED4$Total.Cases
# CASES_DEATHS_RECOVERED4$Total.Décès <- CASES_DEATHS_RECOVERED4$Total.Deaths
# CASES_DEATHS_RECOVERED4$Taux.Cas <- CASES_DEATHS_RECOVERED4$Case.Rate
# CASES_DEATHS_RECOVERED4$Taux.Décès <- CASES_DEATHS_RECOVERED4$Death.Rate
# 
# 
# ### compare the two sources
# sum(CASES_DEATHS_RECOVERED3$Cases, na.rm = TRUE)
# sum(CASES_DEATHS_RECOVERED3$Total.Cases, na.rm = TRUE)
# 
# sum(CASES_DEATHS_RECOVERED3$Deaths, na.rm = TRUE)
# sum(CASES_DEATHS_RECOVERED3$Total.Deaths, na.rm = TRUE)
# 
# 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# 
# # ##get your token to access google drive
# # shiny_token2 <- gs_auth(new_user = TRUE)
# # saveRDS(shiny_token2, "shiny_app_token.rds")
# # #gs_auth(token = "shiny_app_token.rds") # from .rds file
# 
# 
# #set up data sheet in google drive
#  # Data <- gs_new("Data") %>%
#  #   gs_ws_rename(from = "Sheet1", to = "Data")
#  # Data <- Data %>%
#  #   gs_edit_cells(ws = "Data", input = cbind("name", "sender.email", "my.email", "msg", "timestamp"), trim = TRUE)
# 
# 
#  ## Note: for some reason it wont work if first row is blank so I went into the google sheet
# ## and put in some values in the first few rows
# 
# # sheetkey <-  "1rxVKig8hjtOElWeGWunK5c96Oh-aIxpVjuIwf_D6UJM"
# # # Data$sheet_key # you can get your key from Data$sheet_key, don't share your sheet key!
# # Data <- gs_key(sheetkey)
# 
# ############################################################################################################################################                 
# ############################################################################################################################################                 
# 
# 
# 
# ui2 <- 
# 
# ############################################################################################################################################                 
# ### 1ST TAB-PANEL
# 
#   navbarPage(inverse=T,
#             "COVID-19 AFRICA",
#              tabPanel("",
#               fluidPage(#HTML('<meta name="viewport" content="width=1024">'),
#               fluidRow(
#                  column(width = 5, offset = 0, 
#                   "",
#               
#                   radioButtons(inputId = "language1", 
#                                label = (""),
#                                choices = (c("English" = "en", "Français" = "fr")),
#                                inline = TRUE,
#                                selected = ("en")),
#                   
#              
#                   strong(h3(textOutput("reactive_case_count"), align = "right")),
#                  
#                    strong(h3(textOutput("reactive_death_count"), align = "right")),
#                   
#                   strong(h3(textOutput("reactive_recovered_count"), align = "right")),
#                   
#                   hr(),
#                   
#                 
#                   span(h4(textOutput("uicmt13"), style="color:red")),
#                  
#                   
#                   br(),
#                   
#                   dataTableOutput("Region"),
#                   
#                   
#                   
#                   br(),
#                   
#                   br(),
#                   
#                   dataTableOutput("Africa"),
#                   
#                   br(),
#                   
#                   br()
#                  ),
#                  
#                  
#                   column(width=5, offset = 0,
#                   "",
#                   uiOutput("uiburden1"),
#                   
#                   #box(leafletOutput("map", height=675, width = 500))
#                   leafletOutput("map", height=500, width = 500),
#                  
#                   br(),
#                   
#                   plotOutput("cumulative", width = "10%"),
#                   
#                   br(),
#                   
#                   plotOutput("consecutiveCases", width = "10%"),
#                   
#                   br(),
#                   
#                   br(),
#                   
#                   br(),
#                   
#                   plotOutput("consecutiveDeaths", width = "10%"))
#                   ,
#                   
#                  column(width=2, offset = 0,
#                   
#                   span(h6(textOutput("uicmt14")), style="color:red"),
#                   
#                   uiOutput("uicmt15"),
#                   
#                   h6(a("christian.dideagossou@gmail.com", href="mailto:christian.dideagossou@gmail.com")),
#                   
#                   
#                   # uiOutput("uicmt82"),
#                   # 
#                   # h6(a("Johns Hopkins CSSE", 
#                   #   href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")),
#                   
#               
#                   
#                   uiOutput("uicmt83"),
#                   
#                   h6(a("https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization")),
#                   
#                 
#                   
#                   # a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
#                   
#                   hr(),
#                   
#                   h6(a(strong("Code"), href="https://github.com/ddkunda/AFRICA-COVID19-visualization")))
#                   
#                   # 
#                   # ) ,
#                   # 
#                   # column(3, offset = 2,
#                   #        "",
#                   #        fluidRow( 
#                   #          plotOutput("cumulative", height = "100%"),
#                   #          plotOutput("consecutive", height = "100%"),
#                   #       
#                   #         br(),
#                   #         
#                   #        span(textOutput("uicmt14"), style="color:red"),
#                   #        
#                   #        uiOutput("uicmt15"),
#                   #        
#                   #        a("christian.dide-agossou@cuanschutz.edu", href="mailto:christian.dide-agossou@cuanschutz.edu"),
#                   #        
#                   #        uiOutput("uicmt82"),
#                   #        
#                   #        a("Johns Hopkins Center for Systems Science and Engineering", 
#                   #          href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
#                   #        
#                   #        hr(),
#                   #        
#                   #        uiOutput("uicmt83"),
#                   #        
#                   #        a("(1) https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization"),
#                   #        
#                   #        br(),
#                   #        
#                   #       # a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
#                   #        
#                   #        hr(),
#                   #        
#                   #        a(strong("Code"), href="#https://github.com/ddkunda/AFRICA-COVID19-visualization")
#                   #        )
#                  # )
#                  )
#              
#                # fluidRow(
#                #   column(5, offset = 0,
#                #          "",
#                #            uiOutput("uicmt14"),
#                #            uiOutput("uicmt15"),
#                #            
#                #            a("christian.dide-agossou@cuanschutz.edu", href="mailto:christian.dide-agossou@cuanschutz.edu")
#                #  
#                )
#               )
#             
#             #,
#              
# ############################################################################################################################################                 
# # ### 2ND TAB-PANEL
# #              tabPanel(
# #                "Information", 
# #                
# #                radioButtons(inputId = "language8", label = "",
# #                             choices = c("English" = "en", "Français" = "fr"),
# #                             inline = TRUE,
# #                             selected = "en"),
# #                fluidRow(
# #                  column(6,
# #                         uiOutput("uicmt81"),
# #                         hr(),
# #                         
# #                         uiOutput("uicmt82"),
# #                         
# #                         a("Johns Hopkins Center for Systems Science and Engineering", 
# #                           href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
# #                         
# #                         hr(),
# #                         
# #                         uiOutput("uicmt83"),
# #                         
# #                         a("(1) https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization"),
# #                         
# #                         br(),
# #                         
# #                         a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
# #                         
# #                                       
# #                         hr(),
# #                        
# #                          a(strong("Code"), href="#https://github.com/ddkunda/AFRICA-COVID19-visualization"),
# #                         
# #                         br(),
# #                         hr(),
# #                         
# #                         uiOutput("uicmt84"),
# #                         
# #                         uiOutput("uicmt85"),
# #                         
# #                         a("christian.dide-agossou@cuanschutz.edu",
# #                           br(),
# #                           href="mailto:christian.dide-agossou@cuanschutz.edu")
# #                         
# #                  )
# #                  # ,
# #                  # column(4, offset = 1,
# #                  #        uiOutput("uicmt86"),
# #                  #        uiOutput("uicmt87"),
# #                  #        uiOutput("uicmt88"),
# #                  #        uiOutput("uimsg81"),
# #                  #        uiOutput("uicmt89")
# #                  #        
# #                  # )
# #                )
# #              )
#              
# ############################################################################################################################################                 
#  
#  )
# 
# ############################################################################################################################################                 
# 
#   
# 
# 
# 
# 
# server2 <- function(input, output){
#   
# 
# ############################################################################################################################################                 
# ### 1ST TAB-PLOT 
#   
#   # Translation for TAB1
#   tr1 <- function(text){ # translates text into current language
#     sapply(text, function(s) translation[[s]][[input$language1]], USE.NAMES=FALSE)
#   }
#   
#   
#   output$uiburden1 <- renderUI({
#     radioButtons(inputId =  "burden1",
#                 label = "", #tr1("Choose a burden measure:"),
#                 choices = tr1(c("Total.Cases", "Total.Deaths", "Case.Rate", "Death.Rate")),
#                 inline = TRUE,
#                 selected  = tr1("Total.Cases"))
#   })
#   
# 
#   reactive_df = reactive({
#     CASES_DEATHS_RECOVERED3
#   })
#   
#   
#   output$reactive_case_count <- renderText({
#     paste0(prettyNum(sum(reactive_df()$Total.Cases, na.rm = TRUE), big.mark=","), tr1(" total cases"))
#   })
#   
#   output$reactive_death_count <- renderText({
#     paste0(prettyNum(sum(reactive_df()$Total.Deaths, na.rm = TRUE), big.mark=","), tr1(" total deaths"))
#   })
#   
#   output$reactive_recovered_count <- renderText({
#     paste0(prettyNum(sum(reactive_df()$Total.Recovered, na.rm = TRUE), big.mark=","), tr1(" total recovered"))
#   })
#   
#   
# ##################################################################################################################################################################################################
# 
#   output$uicmt13 <- renderText({tr1("The case and death rates are per 1,000,000 people. The countries with the highest number of cases and deaths do not necessarily have the highest case and death rates when considering the size of their populations. Click on the arrows to order the columns.")})
#   
#  
#   
#   
# ##################################################################################################################################################################################################
#   
#   reactive_df1b = reactive({
#     
#     df1b <- CASES_DEATHS_RECOVERED4[, c(tr1("Region"),tr1("Total.Cases"),tr1("Total.Deaths"),tr1("Case.Rate"), tr1("Death.Rate"))]
#     df1b <- df1b[,-2]
#     df1b[order(-(df1b[,c("Total.Cases")])), ]
#   })
#   
#   
#   output$Region <- renderDataTable({
#     datatable(reactive_df1b(),  rownames = FALSE, options = list(paging = FALSE, searching=FALSE, 
#                                                                  #pageLength = 5,
#                                                                  dom="t",
#                                                                  initComplete = JS(
#                                                                    "function(settings, json) {",
#                                                                    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                                                                    "}")))%>%
#       formatCurrency(2:5, currency = "", interval = 3, mark = ",",digits = 0)
#   })
#   
# ##################################################################################################################################################################################################
#   
#   reactive_df1 = reactive({
#     
#     CASES_DEATHS_RECOVERED3 <- CASES_DEATHS_RECOVERED3[, tr1(c("Country","Total.Cases","Total.Deaths","Case.Rate","Death.Rate"))]
#     CASES_DEATHS_RECOVERED3 <- CASES_DEATHS_RECOVERED3[order(-(CASES_DEATHS_RECOVERED3[,c("Total.Cases")])), ]
#   })
#   
#   output$Africa <- renderDataTable({
#    datatable(reactive_df1(), rownames = FALSE, options = list(pageLength = 30,
#                                                               #dom="ft",
#                                                               initComplete = JS(
#                                                                 "function(settings, json) {",
#                                                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
#                                                                 "}")))%>%
#       formatCurrency(2:5, currency = "", interval = 3, mark = ",", digits = 0)
#   })
#   
#   
# ##################################################################################################################################################################################################
#   
#   output$map <- renderLeaflet({
#     
#   
#     req(input$burden1)
#     req(CASES_DEATHS_RECOVERED3)
#    
#     
#     df1 <- CASES_DEATHS_RECOVERED3
#     
#     if(input$burden1 %in% paste(tr1("Total.Cases"))){
#       leaflet(df1)  %>%
#         setView(lng = 15, lat = 0, zoom = 3)%>%
#         addCircleMarkers(df1,
#                          lng = ~Long,
#                          lat = ~Lat,
#                          radius = df1[, input$burden1]*0.00003,
#                          weight = 1,
#                          opacity = 4,
#                          fill = TRUE,
#                          col="blue",
#                          label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
#         addProviderTiles(providers$CartoDB.Positron)
#       }
#     
# #  Stamen.TonerLite 
# # CartoDB.DarkMatter
# # CartoDB.Positron    
# # Stadia.AlidadeSmoothDark
#     
#     else if(input$burden1 %in% paste(tr1("Total.Deaths"))){
#       leaflet(df1)  %>%
#         setView(lng = 15, lat = 0, zoom = 3)%>%
#         addCircleMarkers(df1,
#                          lng = ~Long,
#                          lat = ~Lat,
#                          radius = df1[, input$burden1]*0.0009,
#                          weight = 1,
#                          opacity = 4,
#                          fill = TRUE,
#                          col="red",
#                          label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
#         addProviderTiles(providers$CartoDB.Positron)
#                          }
#    
#     
#   else if(input$burden1 %in% paste(tr1("Case.Rate"))){
#     leaflet(df1)  %>%
#       setView(lng = 15, lat = 0, zoom = 3)%>%
#       addCircleMarkers(df1,
#                        lng = ~Long,
#                        lat = ~Lat,
#                        radius = df1[, input$burden1]*0.0001,
#                        weight = 1,
#                        opacity = 4,
#                        fill = TRUE,
#                        col="blue",
#                        label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1M population"), ")", ":", df1[, input$burden1]))%>%
#       addProviderTiles(providers$CartoDB.Positron)
#   }
#   
#     
#     else if(input$burden1 %in% paste(tr1("Death.Rate"))){
#       leaflet(df1)  %>%
#         setView(lng = 15, lat = 0, zoom = 3)%>%
#         addCircleMarkers(df1,
#                          lng = ~Long,
#                          lat = ~Lat,
#                          radius = df1[, input$burden1]*0.009,
#                          weight = 1,
#                          opacity = 4,
#                          fill = TRUE,
#                          col="red",
#                          label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1M population"),")", ":", df1[, input$burden1]))%>%
#         addProviderTiles(providers$CartoDB.Positron)
#     }
#     
# })
# 
# 
# ##################################################################################################################################################################################################
#  
#    output$cumulative <- renderPlot({
#     
#     req(CASES_DEATHS_long2)
#     
#     ##Subset the necessary columns
#     df2 <- CASES_DEATHS_long2
#     df2 <- df2[,c("real_date", tr1("total cases"), tr1("total deaths"))]
#     df2b <- melt(df2, id=c("real_date"))
#     
#     ggplot(df2b)+ 
#       geom_line(aes(x=real_date, y=value, colour=variable), size=1)+
#       scale_colour_manual(values=c("blue","red"))+
#       xlab("")+
#       ylab(tr1("daily total"))+
#       #scale_x_date(date_breaks = "1 month", date_labels = "%b")+
#       scale_x_date(date_breaks = "8 month", labels = date_format("%b-%y")) +
#       scale_y_continuous(labels = function(x) format(x, scientific = TRUE), limits = c(0,max(df2b$value+500000)))+
#       #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")},limits = c(0,max(df2b$value+500000))) +
#       theme_bw()+
#       theme(panel.background = element_rect(fill = "grey90", colour = "#6D9EC1"))+
#       #theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
#       theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=14))+
#       theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
#       theme(axis.text.x = element_text(size=8, color = "black", face="bold"),
#             axis.text.y = element_text(size=8, color = "black", face="bold"))+
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             axis.line = element_line(colour = "black"))
#     # +
#     # theme(plot.margin = margin(19,50, 50,19))
# 
#     }
#   #, height = 300, width = 400
#   , height = 400, width = 500
# )
#   
# ##################################################################################################################################################################################################
#   
# output$consecutiveCases <- renderPlot({
#     
#     req(sum_CASES_long2_lf3a)
#     df3 <- sum_CASES_long2_lf3a
#     
#     ggplot(df3,  aes(real_date, daily_new_cases))+
#       #geom_bar(stat="identity", col="black", fill="blue")+
#       geom_density(stat = "identity", alpha = 0.3, col="blue")+
#       xlab("")+
#       ylab(tr1("daily new cases"))+
#       #scale_x_date(date_breaks = "1 month", date_labels = "%b")+
#       scale_x_date(date_breaks = "8 month", labels = date_format("%b-%y")) +
#       #ylim(0,max(df3$value+10000))+
#       scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
#       #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
#       #geom_text(aes(label=paste(" ",daily_new_cases)), size=2, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
#       theme_bw()+
#       theme(panel.background = element_rect(fill = "grey90", colour = "#6D9EC1"))+
#       #theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
#       theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=14))+
#       theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
#       theme(axis.text.x = element_text(size=8, color = "black", face="bold"),
#             axis.text.y = element_text(size=8, color = "black", face="bold"))+
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             axis.line = element_line(colour = "black"))
#     # +
#     #   theme(plot.margin = margin(19,50, 50,19))
#     
#   }
#   #, height = 365, width = 450
#   , height = 450, width = 500
#   )
#   
#   ##################################################################################################################################################################################################
# 
#     output$consecutiveDeaths <- renderPlot({
#     
#     req(sum_DEATHS_long2_lf3a)
#     df4 <- sum_DEATHS_long2_lf3a
#     
#     ggplot(df4,  aes(real_date, daily_new_deaths))+
#       #geom_bar(stat="identity", col="black", fill="red")+
#       geom_density(stat = "identity", alpha = 0.3, col="red")+
#       xlab("")+
#       ylab(tr1("daily new deaths"))+
#       #scale_x_date(date_breaks = "1 month", date_labels = "%b")+
#       #ylim(0,max(df4$value+200))+
#       #scale_y_continuous(labels = function(x) format(x, scientific = TRUE))+
#       scale_x_date(date_breaks = "8 month", labels = date_format("%b-%y")) +
#       #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
#       #geom_text(aes(label=paste(" ",daily_new_deaths)), size=2, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
#       theme_bw()+
#       theme(panel.background = element_rect(fill = "grey90", colour = "#6D9EC1"))+
#       #theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
#       theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=14))+
#       theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
#       theme(axis.text.x = element_text(size=8, color = "black", face="bold"),
#             axis.text.y = element_text(size=8, color = "black", face="bold"))+
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#             axis.line = element_line(colour = "black"))
#     # +
#     #   theme(plot.margin = margin(19,50, 50,19))
#     
#   }
#   #, height = 365, width = 450
#   , height = 450, width = 500
#   )
#   
# ##################################################################################################################################################################################################
#   
# 
# output$uicmt14 <- renderText({tr1("Any questions can be sent to:")})
# 
# 
# output$uicmt15 <- renderUI({
#   helpText(h6(strong(("Christian Dide-Agossou, PhD"))))
# })
# 
# 
# # output$uicmt82 <- renderUI({
# #   helpText(h6(tr1("Adapted from timeline data published by: ")))
# # })
# 
# output$uicmt83 <- renderUI({
#   helpText(h6(tr1("Code adapted from the following sources:")))
# })
# 
# ############################################################################################################################################                 
# ### 2ND TAB-PLOT 
#   
#   # ## Translation for TAB7
#   # tr2 <- function(text){
#   #   sapply(text,function(s) translation[[s]][[input$language8]], USE.NAMES=FALSE)
#   # }
#   # 
#   # # UI for TAB8
#   # output$uicmt81 <- renderUI({
#   #   helpText(h4(em(tr2("This Shiny App provides a quick look at the AFRICA COVID-19 data, including total and country-level number of cases and deaths."))))
#   # })
#   # 
#   # output$uicmt82 <- renderUI({
#   #   helpText(tr2("Adapted from timeline data published by: "))
#   # })
#   # 
#   # output$uicmt83 <- renderUI({
#   #   helpText(tr2("Code adapted from the following sources:"))
#   # })
#   # 
#   # 
#   # output$uicmt84 <- renderUI({
#   #   helpText(tr2("Any questions or comments can be sent to:"))
#   # })
#   # 
#   # 
#   # output$uicmt85 <- renderUI({
#   #   helpText(strong(tr2("Christian Dide-Agossou, MS, BS. (PhD Candidate in Epidemiology)")))
#   # })
#   # 
#   # # output$uicmt86 <- renderUI({
#   # #   textInput("name", 
#   # #             tr2("Name:"),
#   # #             value="")
#   # # })
#   # # 
#   # # output$uicmt87 <- renderUI({
#   # #   textInput("from", 
#   # #             tr2("From:"), 
#   # #             value="xxx@gmail.com")
#   # # })
#   # # 
#   # # output$uicmt88 <- renderUI({
#   # #   textInput("to", 
#   # #             tr2("To:"), 
#   # #             value="christian.dide-agossou@cuanschutz.edu")
#   # # })
#   # # 
#   # # output$uimsg81 <- renderUI({
#   # #   textAreaInput("mgs", tr2("Message"), value = "", width = '100%', rows = 5, resize = "both")
#   # # })
#   # # 
#   # # 
#   # # output$uicmt89 <- renderUI({
#   # #   actionButton("send", 
#   # #                tr2("Submit"))
#   # # })
#   # 
#   # 
#   # ## MAINPANEL for TAB8
#   # #store the results
#   # Results <- reactive(c(
#   #   input$name, input$from, input$to, input$mgs, Sys.time()
#   # ))
#   # 
#   # #This will add the new row at the bottom of the dataset in Google Sheets.
#   # observeEvent(input$send, {                                                                 
#   #   Data  <- Data  %>%                                                                      
#   #     gs_add_row(ws = "Data", input = Results())                                                               
#   # })
#   
#   
# ############################################################################################################################################                 
#   
# }
# 
# ############################################################################################################################################                 
# 
# shinyApp(ui2, server2)
# 
# 
# 
# 
