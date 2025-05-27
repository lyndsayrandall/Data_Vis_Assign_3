#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(bslib)
library(future)
library(promises)
library(shinyBS)





options(shiny.autoreload = TRUE)
options(shiny.devmode = TRUE)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  source("mainServer.R") 
  source("moduleUI.R")
  source("utilFun.R")
  source("modalsDispCode.R")
  source("errorPlotsMessages.R")
  source("dmgWRDiffPlot.R")
  
  
  # https://stackoverflow.com/questions/21465411/r-shiny-passing-reactive-to-selectinput-choices
  output$selBattleType <- renderUI({
    selectInput("type",
                "Select Battle Type",
                gameType$Type,
                selected = c("Random"))
  })
  

  output$testPlot <- renderPlotly({test3});
  output$slideTitle <- renderText({slideTitle})
  output$testPlot2 <- renderPlotly({
    

    req(input$type)
    plotdmgWRDiff(gamedata, input$type)
    })
  dispAboutModal(input,"about")
  dispRefModal(input,"references")
  dispAccessModal(input,"dbAccess")
  


 
  
}#EOServer Function



# Define UI for application that draws a histogram
ui <- fixedPage(
  
    tags$head(
      tags$script(HTML("
        function resizeWindow() {
          window.resizeTo(1920,1080);
        };
      "))
    ),
    tags$body(tags$script(HTML("onload= 'resizeWindow()'"))),
    
   

    # Application title
    titlePanel(textOutput("slideTitle")),
    # Drop Down
    
    tabsetPanel(
      tabPanel("Battle Type",
               sidebarLayout(
                 sidebarPanel(
                   "Sidebar",
                   width = 2,
                   uiOutput("selBattleType")),
                 # Show a plot of the generated distribution
                 mainPanel(
                   
                   plotlyOutput("testPlot2")),
                 position = c("right")   
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
