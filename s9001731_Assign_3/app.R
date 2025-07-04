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
library(shinyWidgets)
library(shinytitle)

source("mainServer.R") 
source("moduleUI.R")
source("modalsDispCode.R")
source("toolTipAlertText.R")
source("utilFun.R")


options(shiny.autoreload = TRUE)
options(shiny.devmode = TRUE)




# Server Logic
server <- function(input, output, session) {
  
  shinyalert("Overview",
             type = "info",
             overview,
             html = TRUE)
  #Slide Title
  output$slideTitle <- renderText({slideTitle})
  output$slideImage <- renderImage({slideImage})
  
  #For Tab 1
  output$tab2Title <- renderText({
    req(input$queryTypeBattle2,
        input$queryTypeShip2,
        input$queryAttr2)
    tab_2_title(input$queryTypeBattle2,input$queryTypeShip2,
                input$queryAttr2)
    })
  
  output$attr_rad_but2 <- renderUI({attrRadioButton2})
  output$sel_type_battle2 <- renderUI({selTypeBattle2})
  output$sel_type_ship2 <- renderUI({selTypeShip2})
  
  output$warningText <- renderUI({selAttribute})
  
  output$timeWR <- renderPlotly({
    input$queryTypeBattle2
    input$queryTypeShip2
    input$queryAttr2
    req(input$queryTypeBattle2,
        input$queryTypeShip2,
        input$queryAttr2)
    queryType2 <- if_else(input$queryAttr2 == "Battle.Type",
                          input$queryTypeBattle2,
                          input$queryTypeShip2)
    # Create 0-row data frame which will be used to store data
    dat <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.3)
      }
    })
    plotCumWin(gameData_human, queryType2,input$queryAttr2)
  });
  output$timeTotal <- renderPlotly({
    input$queryTypeBattle2
    input$queryTypeShip2
    input$queryAttr2
    req(input$queryTypeBattle2,
        input$queryTypeShip2,
        input$queryAttr2)
    queryType2 <- if_else(input$queryAttr2 == "Battle.Type",
                          input$queryTypeBattle2,
                          input$queryTypeShip2)
    plotWinTot(gameData_human, queryType2,input$queryAttr2)
    
  });
  
  
  # For Tab2
  
  output$tab1Title <- renderText({
          req(input$queryTypeBattle,
              input$queryTypeShip,
              input$queryAttr)
          tab_1_title(input$queryTypeBattle,input$queryTypeShip,
                           input$queryAttr)})
  
  output$attr_rad_but <- renderUI({attrRadioButton})
  output$sel_type_battle <- renderUI({selTypeBattle})
  output$sel_type_ship <- renderUI({selTypeShip})
  
  output$scatterWRDMG <- renderPlotly({
    req(input$queryTypeBattle,
        input$queryTypeShip,
        input$queryAttr)
    queryType <- if_else(input$queryAttr == "Battle.Type",
                         input$queryTypeBattle,
                         input$queryTypeShip)
    # Create 0-row data frame which will be used to store data
    dat2 <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat2, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.3)
      }
    })
    plotdmgWRDiff(gameData_human, queryType,input$queryAttr)
  })
  
  output$histDensWR <- renderPlotly({
    req(input$queryTypeBattle,
        input$queryTypeShip,
        input$queryAttr)
    queryType <- if_else(input$queryAttr == "Battle.Type",
                         input$queryTypeBattle,
                         input$queryTypeShip)
    plotdmgWRHistDens(gameData_human, queryType,input$queryAttr)[[1]]
  })
  
  output$histDensDMG <- renderPlotly({
    req(input$queryTypeBattle,
        input$queryTypeShip,
        input$queryAttr)
    queryType <- if_else(input$queryAttr == "Battle.Type",
                         input$queryTypeBattle,
                         input$queryTypeShip)
    plotdmgWRHistDens(gameData_human, queryType,input$queryAttr)[[2]]
  })
  # For Tab 3
  
  output$tab3Title <- renderText({
    req(input$queryTypeBattle3,
        input$queryTypeShip3,
        input$queryAttr3)
    tab_3_title(input$queryTypeBattle3,input$queryTypeShip3,
                input$queryAttr3)})
  
  output$plotbyhour <- renderPlotly({
    req(input$queryTypeBattle3,
        input$queryTypeShip3,
        input$queryAttr3)
    queryType <- if_else(input$queryAttr3 == "Battle.Type",
                         input$queryTypeBattle3,
                         input$queryTypeShip3)
    # Create 0-row data frame which will be used to store data
    dat3 <- data.frame(x = numeric(0), y = numeric(0))
    withProgress(message = 'Making plot', value = 0, {
      # Number of times we'll go through the loop
      n <- 10
      
      for (i in 1:n) {
        # Each time through the loop, add another row of data. This is
        # a stand-in for a long-running computation.
        dat <- rbind(dat3, data.frame(x = rnorm(1), y = rnorm(1)))
        
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing part", i))
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.3)
      }
    })
    plotByHour(gameData_human, queryType,input$queryAttr3)
  })
  
  output$plotbyday <- renderPlotly({
    req(input$queryTypeBattle3,
        input$queryTypeShip3,
        input$queryAttr3)
    queryType <- if_else(input$queryAttr3 == "Battle.Type",
                         input$queryTypeBattle3,
                         input$queryTypeShip3)
    plotByDay(gameData_human, queryType,input$queryAttr3)
  })
  
  
  
  
  
  output$attr_rad_but3 <- renderUI({attrRadioButton3})
  output$sel_type_battle3 <- renderUI({selTypeBattle3})
  output$sel_type_ship3 <- renderUI({selTypeShip3})
  
 
  
  # For common footer
  dispAboutModal(input,"about")
  dispRefModal(input,"references")
  dispAccessModal(input,"dbAccess")
  dispScreenshotAccessPng(input,"screenshotAccess")
  dispScreenshotPAPng(input,"screenshotPA")

}#EOServer Function



# Client UI logic
ui <- fixedPage(
   #theme = bs_theme(version =5),
   withMathJax(),
   useShinyjs(),
   title = "WOWS DATA tertius_keen",
   use_shiny_title(),
   busy_window_title(),
   tags$head(
     tags$link(rel = "stylesheet", type = "text/css", href = "stylesheet.css")
     
   ),
   
   tags$head(
     tags$script(HTML("
      function resizeWindow(width, height) {
        window.resizeTo(width, height);
      }
      function sizes() {
  const contentWidth = [...document.body.children].reduce( 
    (a, el) => Math.max(a, el.getBoundingClientRect().right), 0) 
    - document.body.getBoundingClientRect().x;

  return {
    windowWidth:  document.documentElement.clientWidth,
    windowHeight: document.documentElement.clientHeight,
    pageWidth:    Math.min(document.body.scrollWidth, contentWidth),
    pageHeight:   document.body.scrollHeight,
    screenWidth:  window.screen.width,
    screenHeight: window.screen.height,
    pageX:        document.body.getBoundingClientRect().x,
    pageY:        document.body.getBoundingClientRect().y,
    screenX:     -window.screenX,
    screenY:     -window.screenY - (window.outerHeight-window.innerHeight),
  }
}
      $(document).on('shiny:connected', function() {
        resizeWindow(1920,1080);
        console.log(sizes());
      });
    "))
   ),
   

    # Application title
    titlePanel(textOutput("slideTitle")),
               
    # Drop Down
    
    tabsetPanel(
     
     tabPanel(title = "Win Rate over Time",
              sidebarLayout(
                sidebarPanel(
                  tags$div(HTML(paste("<h4><b>",textOutput("tab2Title"),
                                      "</b></h4><br>"))),
                  uiOutput("attr_rad_but2"),
                  uiOutput("sel_type_battle2"),
                  uiOutput("sel_type_ship2"),
                  tags$div(HTML(paste(uiOutput("warningText"))),
                           style = "font-size: 6px;
                                    border-style:solid;
                                    border- radius: 10px;
                                    border-color: black;
                                    background-color:Tomato;
                                    padding : 5px;
                                    border-width: 2px;
                                    color : white;"),
                  width = 2,
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  card(
                    plotlyOutput("timeWR")
                  ),
                  card(
                    plotlyOutput("timeTotal")
                  ),
                  width = 10,
                  style = "margin: 0 ;"
                ),
                
                position = c("left")   
              )),
     
     tabPanel(title = "Scatter, Histogram, Density",
              sidebarLayout(
                sidebarPanel(
                  tags$div(HTML(paste("<h4><b>",textOutput("tab1Title"),
                                      "</b></h4><br>"))),
                  
                  uiOutput("attr_rad_but"),
                  
                  uiOutput("sel_type_battle"),
                  uiOutput("sel_type_ship"),
                  width = 2,
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  card(
                    plotlyOutput("scatterWRDMG")
                  ),
                  layout_columns(
                    card(
                      plotlyOutput("histDensWR")),
                    card(
                      plotlyOutput("histDensDMG")
                    ),
                    col_widths = c(6,6)
                  ),
                  width = 10,
                ),
                position = c("left")   
              )),
     
     tabPanel( title = "By Hour and Day",
       sidebarLayout(
           sidebarPanel(
             tags$div(HTML(paste("<h4><b>",textOutput("tab3Title"),
                                 "</b></h4><br>"))),
             
             uiOutput("attr_rad_but3"),
             
             uiOutput("sel_type_battle3"),
             uiOutput("sel_type_ship3"),
             width = 2,
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             card(
               plotlyOutput("plotbyhour")
             ),
            
             card(
               plotlyOutput("plotbyday")
             ),  
             width = 10,
           ),
           position = c("left")   
         )),
    
    ),
   
   
    tags$footer(
      style = ("position:fixed; left:0; bottom:0;"),
      actionButton("about", "About") ,
      actionButton("references", "References") ,
      actionButton("dbAccess", "Access Database Structure"),
      actionButton("screenshotAccess", "Screenshot Access Input"),
      actionButton("screenshotPA", "Screenshot Potato Alert") ),
      
   
    )
    





# Run the application 
shinyApp(ui = ui, server = server )


