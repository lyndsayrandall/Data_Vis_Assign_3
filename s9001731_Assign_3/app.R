# App.R
# Author: Mark Randall
# Date: 11 June 2025
# Engine for Shiny App

library(shiny)
library(shinyjs)
library(plotly)
library(bslib)
library(future)
library(promises)
library(shinyBS)
library(shinyalert)

source("mainServer.R") 
source("moduleUI.R")
source("utilFun.R")
source("modalsDispCode.R")
source("toolTipAlertText.R")



options(shiny.autoreload = TRUE)
options(shiny.devmode = TRUE)

#shipType <- read.xlsx("../Data/Ship Type.xlsx")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  shinyalert("Overview",
             type = "info",
             overview,
             html = TRUE)
  
  output$attr_rad_but <- renderUI({attrRadioButton})
  output$sel_type_battle <- renderUI({selTypeBattle})
  output$sel_type_ship <- renderUI({selTypeShip})
  
 
  output$testPlot <- renderPlotly({test3});
  output$slideTitle <- renderText({slideTitle})
  
  
  output$testPlot2 <- renderPlotly({
     req(input$queryTypeBattle,
         input$queryTypeShip,
         input$queryAttr)
    queryType <- if_else(input$queryAttr == "Battle.Type",
                         input$queryTypeBattle,
                         input$queryTypeShip)
    print(paste("query type",queryType)) 
    print(paste("query Attr", input$queryAttr ))
    plotdmgWRDiff(gameData_human, queryType,input$queryAttr)
    })
  
  output$testPlot3 <- renderPlotly({
     req(input$queryTypeBattle,
         input$queryTypeShip,
         input$queryAttr)
  queryType <- if_else(input$queryAttr == "Battle.Type",
                       input$queryTypeBattle,
                       input$queryTypeShip)
  print(paste("query type",queryType)) 
  print(paste("query Attr", input$queryAttr ))
  plotdmgWRHistDens(gameData_human, queryType,input$queryAttr)
  })
  dispAboutModal(input,"about")
  dispRefModal(input,"references")
  dispAccessModal(input,"dbAccess")
  


 
  
}#EOServer Function



# Define UI for application that draws a histogram
ui <- fixedPage(
   useShinyjs(),
    
    tags$head(
      tags$script(HTML("
        function resizeWindow() {
          window.resizeTo(1920,1080);
        };
      "))
    ),
    tags$body(tags$script(HTML("onload= 'resizeWindow()'"))),
    withMathJax(),
   

    # Application title
    titlePanel(textOutput("slideTitle")),
    # Drop Down
    
    tabsetPanel(
      tabPanel("Scatter, Histogram, Density",
                 sidebarLayout(
                   sidebarPanel(
                   uiOutput("attr_rad_but"),
                   uiOutput("sel_type_battle"),
                   uiOutput("sel_type_ship"),
                   width = 2,
                    ),
                   
                 # Show a plot of the generated distribution
                 mainPanel(
                   card(
                     plotlyOutput("testPlot2")
                   ),
                   card(
                     plotlyOutput("testPlot3")
                   )
                   ),
                 position = c("left")   
               )),
     tabPanel("Page 2"),
     tabPanel("Page 3")
    ),
   
   
    tags$footer(
      style = ("position:fixed; left:0; bottom:0;"),
      actionButton("about", "About") ,
      actionButton("references", "References") ,
            actionButton("dbAccess", "Access Database Structure"))
   
    )
    





# Run the application 
shinyApp(ui = ui, server = server)

runGadget(shinyApp(ui = ui, server = server) , 
          viewer =browserViewer(browser = getOption("browser")))
