# errorPlotsMessages.R
# Author: Mark Randall
# Date: 11 June 2025
# Creates a plot for no data in data frame



library('RCurl')
library(png)
errorPlotFig <- function() {
  # To render stand alone put s9001731_Assign_3 to path
  image_file <- "Images/HMS_Ark_Royal_sinking_2.jpg"
  txt <- RCurl::base64Encode(readBin(image_file, "raw", 
                                     file.info(image_file)[1, "size"]), "txt")
  
  
  errFig <- plot_ly(x = c(0, 0.5, 1, 2, 2.2), 
                         y = c(1.23, 2.5, 0.42, 3, 1), 
                         type = 'scatter', 
                         mode = 'lines+markers',
                         opacity = 0.0
                         ) %>%
    layout( images = list(list(
                      source =  paste('data:image/png;base64', txt, sep=','),
                      xref = "x",
                      yref = "y",
                      x = 0,
                      y = 3,
                      sizex = 2,
                      sizey = 2,
                      sizing = "stretch",
                      opacity = 1.0,
                      layer = "below" )),
            xaxis = list(title = "",
                         zeroline = FALSE,
                         showgrid = FALSE,
                         showticklabels=FALSE),
            yaxis = list(title = "",
                         zeroline = FALSE,
                         showgrid = FALSE,
                         showticklabels=FALSE),
            annotations = list(
                            list( x = 1.8,
                                  y = 0.8,
                                  xref = "x",
                                  yref = "y",
                                  text = "(Beadell,1941)",
                                  showarrow = c(FALSE)),
                            list( x = 1.0,
                                  y = 0.5,
                                  xref = "x",
                                  yref = "y",
                                  text = "No Information Available",
                                  showarrow = c(FALSE),
                                  font = list(size = 24))
            )) %>%
             config(displayModeBar = FALSE)

  
  return(errFig)
}#EOFn

