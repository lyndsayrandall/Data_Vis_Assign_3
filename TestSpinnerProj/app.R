#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# 
# library(shiny)
# server <- function(input, output) {
#   output$plot <- renderPlot({
#     input$goPlot # Re-run when button is clicked
#     
#     # Create 0-row data frame which will be used to store data
#     dat <- data.frame(x = numeric(0), y = numeric(0))
#     
#     withProgress(message = 'Making plot', value = 0, {
#       # Number of times we'll go through the loop
#       n <- 10
#       
#       for (i in 1:n) {
#         # Each time through the loop, add another row of data. This is
#         # a stand-in for a long-running computation.
#         dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
#         
#         # Increment the progress bar, and update the detail text.
#         incProgress(1/n, detail = paste("Doing part", i))
#         
#         # Pause for 0.1 seconds to simulate a long computation.
#         Sys.sleep(0.1)
#       }
#     })
#     
#     plot(dat$x, dat$y)
#   })
# }
# 
# ui <- shinyUI(basicPage(
#   plotOutput('plot', width = "300px", height = "300px"),
#   actionButton('goPlot', 'Go plot')
# ))
# 
# shinyApp(ui = ui, server = server)


# ui <- fluidPage(
#   windowWidth = 1920,
#   windowHeight = 1080,
#   shinybrowser::detect()
# )
# 
# server <- function(input, output, session) {
#   observe({
#     str(shinybrowser::get_all_info())
#   })
# }
# 
# shinyApp(ui, server)

library(shiny)

ui <- fluidPage(
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
        resizeWindow(1080,1920);
        console.log(sizes());
      });
    "))
  ),
  titlePanel("Resize Window Example"),
  p("This app window will resize to 800x600 when it connects.")
)
server <- function(input, output) {
}

#shinyApp(ui, server)

runGadget(shinyApp(ui = ui, server = server) , 
          viewer =browserViewer(browser = getOption("browser")))
