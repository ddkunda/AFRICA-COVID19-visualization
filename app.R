stringsAsFactors = FALSE
#™Chris Dide-Agossou
############################################################################################################################################                 
############################################################################################################################################                 

## load data
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
#library(rsconnect)
library(reshape2)
library(leaflet)
library(leaflet.extras)
library(DT)
library(XML)
library(RCurl)
library(rvest)


######################################################################################################################################################################################                
######################################################################################################################################################################################                

source("../AFRICA_COVID19_visualization/dataset.R")
source("../AFRICA_COVID19_visualization/dictionary.R")

## load AfricaCDC data
load("../AFRICA_COVID19_visualization/CASES_DEATHS_v3.RData")
load("../AFRICA_COVID19_visualization/coronavirus_today.RData")
load("../AFRICA_COVID19_visualization/coronavirus_continent.RData")

## import country and DiseaseGroup translation
load("../AFRICA_COVID19_visualization/translation.bin") 
country_translation <- read.csv("../AFRICA_COVID19_visualization/country_translation.csv", header = TRUE)


### make summary table for region and calculate case and death rates

CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED3[, c("Region","TotalCases", "TotalDeaths", "Pop.")]
CASES_DEATHS_RECOVERED4 <- CASES_DEATHS_RECOVERED4%>%
  group_by(Region)%>%
  summarise_if(is.numeric, funs(sum), na.rm=TRUE)

CASES_DEATHS_RECOVERED4 <- as.data.frame(CASES_DEATHS_RECOVERED4)
CASES_DEATHS_RECOVERED4$Case.Rate <- round(((CASES_DEATHS_RECOVERED4$TotalCases/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)
CASES_DEATHS_RECOVERED4$Death.Rate <- round(((CASES_DEATHS_RECOVERED4$TotalDeaths/CASES_DEATHS_RECOVERED4$Pop.)*1000000),2)


## french columns
CASES_DEATHS_RECOVERED4$Région <- CASES_DEATHS_RECOVERED4$Region
CASES_DEATHS_RECOVERED4$CasTotal <- CASES_DEATHS_RECOVERED4$TotalCases
CASES_DEATHS_RECOVERED4$DécèsTotal <- CASES_DEATHS_RECOVERED4$TotalDeaths
CASES_DEATHS_RECOVERED4$Tx.Cas <- CASES_DEATHS_RECOVERED4$Case.Rate
CASES_DEATHS_RECOVERED4$Tx.Mortalité <- CASES_DEATHS_RECOVERED4$Death.Rate




### compare the two sources
sum(CASES_DEATHS_RECOVERED3$Cases, na.rm = TRUE)
sum(CASES_DEATHS_RECOVERED3$TotalCases, na.rm = TRUE)

sum(CASES_DEATHS_RECOVERED3$Deaths, na.rm = TRUE)
sum(CASES_DEATHS_RECOVERED3$TotalDeaths, na.rm = TRUE)



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
            "COVID-19 AFRICA",
             tabPanel("",
              fluidPage(#HTML('<meta name="viewport" content="width=1024">'),
              fluidRow(
                 column(width = 5, offset = 0, 
                  "",
              
                  radioButtons(inputId = "language1", 
                               label = (""),
                               choices = (c("English" = "en", "Français" = "fr")),
                               inline = TRUE,
                               selected = ("en")),
                  
             
                  strong(h3(textOutput("reactive_case_count"), align = "right")),
                 
                   strong(h3(textOutput("reactive_death_count"), align = "right")),
                  
                  strong(h3(textOutput("reactive_recovered_count"), align = "right")),
                  
                  hr(),
                  
                
                  span(h4(textOutput("uicmt13"), style="color:red")),
                 
                  
                  br(),
                  
                  dataTableOutput("Africa"),
                  
                  br(),
                  
                  br(),
                  
                  dataTableOutput("Region"),
                  
                  br(),
                  
                  br()
                 ),
                 
                 
                  column(width=5, offset = 0,
                  "",
                  uiOutput("uiburden1"),
                  
                  #box(leafletOutput("map", height=675, width = 500))
                  leafletOutput("map", height=500, width = 500),
                 
                  br(),
                  
                  plotOutput("cumulative", width = "10%"),
                  
                  br(),
                  
                  plotOutput("consecutive", width = "10%"))
                  
                  ,
                  
                 column(width=2, offset = 0,
                  
                  span(h6(textOutput("uicmt14")), style="color:red"),
                  
                  uiOutput("uicmt15"),
                  
                  h6(a("christian.dide-agossou@cuanschutz.edu", href="mailto:christian.dide-agossou@cuanschutz.edu")),
                  
                  
                  uiOutput("uicmt82"),
                  
                  h6(a("Johns Hopkins CSSE", 
                    href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series")),
                  
              
                  
                  uiOutput("uicmt83"),
                  
                  h6(a("https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization")),
                  
                
                  
                  # a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
                  
                  hr(),
                  
                  h6(a(strong("Code"), href="https://github.com/ddkunda/AFRICA-COVID19-visualization")))
                  
                  # 
                  # ) ,
                  # 
                  # column(3, offset = 2,
                  #        "",
                  #        fluidRow( 
                  #          plotOutput("cumulative", height = "100%"),
                  #          plotOutput("consecutive", height = "100%"),
                  #       
                  #         br(),
                  #         
                  #        span(textOutput("uicmt14"), style="color:red"),
                  #        
                  #        uiOutput("uicmt15"),
                  #        
                  #        a("christian.dide-agossou@cuanschutz.edu", href="mailto:christian.dide-agossou@cuanschutz.edu"),
                  #        
                  #        uiOutput("uicmt82"),
                  #        
                  #        a("Johns Hopkins Center for Systems Science and Engineering", 
                  #          href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
                  #        
                  #        hr(),
                  #        
                  #        uiOutput("uicmt83"),
                  #        
                  #        a("(1) https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization"),
                  #        
                  #        br(),
                  #        
                  #       # a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
                  #        
                  #        hr(),
                  #        
                  #        a(strong("Code"), href="#https://github.com/ddkunda/AFRICA-COVID19-visualization")
                  #        )
                 # )
                 )
             
               # fluidRow(
               #   column(5, offset = 0,
               #          "",
               #            uiOutput("uicmt14"),
               #            uiOutput("uicmt15"),
               #            
               #            a("christian.dide-agossou@cuanschutz.edu", href="mailto:christian.dide-agossou@cuanschutz.edu")
               #  
               )
              )
            
            #,
             
############################################################################################################################################                 
# ### 2ND TAB-PANEL
#              tabPanel(
#                "Information", 
#                
#                radioButtons(inputId = "language8", label = "",
#                             choices = c("English" = "en", "Français" = "fr"),
#                             inline = TRUE,
#                             selected = "en"),
#                fluidRow(
#                  column(6,
#                         uiOutput("uicmt81"),
#                         hr(),
#                         
#                         uiOutput("uicmt82"),
#                         
#                         a("Johns Hopkins Center for Systems Science and Engineering", 
#                           href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series"),
#                         
#                         hr(),
#                         
#                         uiOutput("uicmt83"),
#                         
#                         a("(1) https://github.com/ddkunda/WHO-data-visualization", href="https://github.com/ddkunda/WHO-data-visualization"),
#                         
#                         br(),
#                         
#                         a("(2) https://github.com/eparker12/nCoV_tracker/blob/master/app.R", href="https://github.com/eparker12/nCoV_tracker/blob/master/app.R"),
#                         
#                                       
#                         hr(),
#                        
#                          a(strong("Code"), href="#https://github.com/ddkunda/AFRICA-COVID19-visualization"),
#                         
#                         br(),
#                         hr(),
#                         
#                         uiOutput("uicmt84"),
#                         
#                         uiOutput("uicmt85"),
#                         
#                         a("christian.dide-agossou@cuanschutz.edu",
#                           br(),
#                           href="mailto:christian.dide-agossou@cuanschutz.edu")
#                         
#                  )
#                  # ,
#                  # column(4, offset = 1,
#                  #        uiOutput("uicmt86"),
#                  #        uiOutput("uicmt87"),
#                  #        uiOutput("uicmt88"),
#                  #        uiOutput("uimsg81"),
#                  #        uiOutput("uicmt89")
#                  #        
#                  # )
#                )
#              )
             
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
                label = "", #tr1("Choose a burden measure:"),
                choices = tr1(c("TotalCases", "TotalDeaths", "Case.Rate", "Death.Rate")),
                inline = TRUE,
                selected  = tr1("TotalCases"))
  })
  

  reactive_df = reactive({
    CASES_DEATHS_RECOVERED3
  })
  
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_df()$TotalCases, na.rm = TRUE), big.mark=","), tr1(" total cases"))
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_df()$TotalDeaths, na.rm = TRUE), big.mark=","), tr1(" total deaths"))
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_df()$TotalRecovered, na.rm = TRUE), big.mark=","), tr1(" total recovered"))
  })
  
  
##################################################################################################################################################################################################

  output$uicmt13 <- renderText({tr1("The case and death rates are per 1,000,000 people. The countries with the highest number of cases and deaths do not necessarily have the highest case and death rates when considering the size of their populations. Click on the arrows to order the columns.")})
  
 
  
   reactive_df1 = reactive({
    
    CASES_DEATHS_RECOVERED3 <- CASES_DEATHS_RECOVERED3[, tr1(c("Country","TotalCases","TotalDeaths","Case.Rate", "Death.Rate"))]
    CASES_DEATHS_RECOVERED3 <- CASES_DEATHS_RECOVERED3[order(CASES_DEATHS_RECOVERED3[,1]), ]
  })
  
  output$Africa <- renderDataTable({
   datatable(reactive_df1(), rownames = FALSE, options = list(pageLength = 10,
                                                              #dom="ft",
                                                              initComplete = JS(
                                                                "function(settings, json) {",
                                                                "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                "}")))
  })
  
  
  
  reactive_df1b = reactive({
   
    df1b <- CASES_DEATHS_RECOVERED4[, c(tr1("Region"),tr1("TotalCases"),tr1("TotalDeaths"),tr1("Case.Rate"), tr1("Death.Rate"))]
    df1b <- df1b[,-2]
  })
  
  
  
##################################################################################################################################################################################################

  output$Region <- renderDataTable({
    datatable(reactive_df1b(),  rownames = FALSE, options = list(paging = FALSE, searching=FALSE, 
                                                                 #pageLength = 5,
                                                                 dom="t",
                                                                 initComplete = JS(
                                                                   "function(settings, json) {",
                                                                   "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                                                   "}")))
  })
  
  
  
##################################################################################################################################################################################################
  output$map <- renderLeaflet({
    
  
    req(input$burden1)
    req(CASES_DEATHS_RECOVERED3)
   
    
    df1 <- CASES_DEATHS_RECOVERED3
    
    if(input$burden1 %in% paste(tr1("TotalCases"))){
      leaflet(df1)  %>%
        setView(lng = 15, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*0.005,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="blue",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.Positron)
      }
    
#  Stamen.TonerLite 
# CartoDB.DarkMatter
# CartoDB.Positron    
# Stadia.AlidadeSmoothDark
    
    else if(input$burden1 %in% paste(tr1("TotalDeaths"))){
      leaflet(df1)  %>%
        setView(lng = 15, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*0.07,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="red",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, ")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.Positron)
                         }
   
    
  else if(input$burden1 %in% paste(tr1("Case.Rate"))){
    leaflet(df1)  %>%
      setView(lng = 15, lat = 0, zoom = 3)%>%
      addCircleMarkers(df1,
                       lng = ~Long,
                       lat = ~Lat,
                       radius = df1[, input$burden1]*0.02,
                       weight = 1,
                       opacity = 4,
                       fill = TRUE,
                       col="blue",
                       label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1 million population"), ")", ":", df1[, input$burden1]))%>%
      addProviderTiles(providers$CartoDB.Positron)
  }
  
    
    else if(input$burden1 %in% paste(tr1("Death.Rate"))){
      leaflet(df1)  %>%
        setView(lng = 15, lat = 0, zoom = 3)%>%
        addCircleMarkers(df1,
                         lng = ~Long,
                         lat = ~Lat,
                         radius = df1[, input$burden1]*1.5,
                         weight = 1,
                         opacity = 4,
                         fill = TRUE,
                         col="red",
                         label = paste(tr1(paste(df1$Country)), "(", input$burden1, tr1("per 1 million population"),")", ":", df1[, input$burden1]))%>%
        addProviderTiles(providers$CartoDB.Positron)
    }
    
})


##################################################################################################################################################################################################
  
  output$cumulative <- renderPlot({
    
    req(CASES_DEATHS_long2)
    
    ##Subset the necessary columns
    df2 <- CASES_DEATHS_long2
    df2 <- df2[,c("real_date", tr1("total cases"), tr1("total deaths"))]
    df2b <- melt(df2, id=c("real_date"))
    
    ggplot(df2b)+ 
      geom_line(aes(x=real_date, y=value, colour=variable), size=1.5)+
      scale_colour_manual(values=c("blue","red"))+
      xlab("date")+
      ylab(tr1("count"))+
      ylim(0,max(df2b$value+10000))+
      #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
      theme_bw()+
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
      theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=10))+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      theme(axis.text.x = element_text(size=5, color = "black", face="bold"),
            axis.text.y = element_text(size=8, color = "black", face="bold"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    # +
    # theme(plot.margin = margin(19,50, 50,19))

    }
  #, height = 300, width = 400
  , height = 400, width = 500
)
  
  
##################################################################################################################################################################################################
  
  
output$consecutive <- renderPlot({
    
    req(sum_CASES_long2_lf3a)
    df3 <- sum_CASES_long2_lf3a
    
    ggplot(df3,  aes(real_date, daily_new_cases))+
      geom_bar(stat="identity", col="black", fill="orange")+
      xlab("date")+
      ylab(tr1("new cases"))+
      geom_text(aes(label=daily_new_cases), size=2.5, position=position_dodge(width=0.5), hjust = "outward", angle = 90)+
      #scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
      theme_bw()+
      theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"))+
      theme(legend.title = element_blank(), legend.position = "top", axis.title = element_text(family = "", color="black", face="bold", size=10))+
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))+
      theme(axis.text.x = element_text(size=5, color = "black", face="bold"),
            axis.text.y = element_text(size=8, color = "black", face="bold"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"))
    # +
    #   theme(plot.margin = margin(19,50, 50,19))
    
  }
  #, height = 365, width = 450
  , height = 400, width = 500
  )
  
# output$uicmt14 <- renderUI({
#   helpText(tr1("Any questions or comments can be sent to:"))
# })

  
##################################################################################################################################################################################################
  

output$uicmt14 <- renderText({tr1("Any questions can be sent to:")})


output$uicmt15 <- renderUI({
  helpText(h6(strong(tr1("Christian Dide-Agossou, MS, (PhD Candidate in Epidemiology)"))))
})


output$uicmt82 <- renderUI({
  helpText(h6(tr1("Adapted from timeline data published by: ")))
})

output$uicmt83 <- renderUI({
  helpText(h6(tr1("Code adapted from the following sources:")))
})

############################################################################################################################################                 
### 2ND TAB-PLOT 
  
  # ## Translation for TAB7
  # tr2 <- function(text){
  #   sapply(text,function(s) translation[[s]][[input$language8]], USE.NAMES=FALSE)
  # }
  # 
  # # UI for TAB8
  # output$uicmt81 <- renderUI({
  #   helpText(h4(em(tr2("This Shiny App provides a quick look at the AFRICA COVID-19 data, including total and country-level number of cases and deaths."))))
  # })
  # 
  # output$uicmt82 <- renderUI({
  #   helpText(tr2("Adapted from timeline data published by: "))
  # })
  # 
  # output$uicmt83 <- renderUI({
  #   helpText(tr2("Code adapted from the following sources:"))
  # })
  # 
  # 
  # output$uicmt84 <- renderUI({
  #   helpText(tr2("Any questions or comments can be sent to:"))
  # })
  # 
  # 
  # output$uicmt85 <- renderUI({
  #   helpText(strong(tr2("Christian Dide-Agossou, MS, BS. (PhD Candidate in Epidemiology)")))
  # })
  # 
  # # output$uicmt86 <- renderUI({
  # #   textInput("name", 
  # #             tr2("Name:"),
  # #             value="")
  # # })
  # # 
  # # output$uicmt87 <- renderUI({
  # #   textInput("from", 
  # #             tr2("From:"), 
  # #             value="xxx@gmail.com")
  # # })
  # # 
  # # output$uicmt88 <- renderUI({
  # #   textInput("to", 
  # #             tr2("To:"), 
  # #             value="christian.dide-agossou@cuanschutz.edu")
  # # })
  # # 
  # # output$uimsg81 <- renderUI({
  # #   textAreaInput("mgs", tr2("Message"), value = "", width = '100%', rows = 5, resize = "both")
  # # })
  # # 
  # # 
  # # output$uicmt89 <- renderUI({
  # #   actionButton("send", 
  # #                tr2("Submit"))
  # # })
  # 
  # 
  # ## MAINPANEL for TAB8
  # #store the results
  # Results <- reactive(c(
  #   input$name, input$from, input$to, input$mgs, Sys.time()
  # ))
  # 
  # #This will add the new row at the bottom of the dataset in Google Sheets.
  # observeEvent(input$send, {                                                                 
  #   Data  <- Data  %>%                                                                      
  #     gs_add_row(ws = "Data", input = Results())                                                               
  # })
  
  
############################################################################################################################################                 
  
}

############################################################################################################################################                 

shinyApp(ui2, server2)




