# dmgWRHistDensPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the faceted Histogram and density plots for average WR Differential
# and average Damage Differential


plotdmgWRHistDens <- function(gamedata, attrType){
  
  typeDF <- retTypeDF(gamedata,attrType)
  
  # attrType <- "Random"
  # typeDF<- retTypeDF(gameData_human,attrType)
  
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  quartTypeDF <- quantile(typeDF$WR.Differential, prob= c(.25,.5,.75),
                          type = 1, na.rm =TRUE)
  
  attrDFIQR <- quartTypeDF[[3]] - quartTypeDF[[1]]
  
  densPlotXLim <- c((quartTypeDF[[1]] - 3*(attrDFIQR)),
                    (quartTypeDF[[3]] + 3*(attrDFIQR)))
  
  
  histDensVars <- c("WR.Differential","DMG.Differential")
  
  histDistPlot_GG <-  ggplot(typeDF,aes( x= WR.Differential)) +
                      geom_histogram(aes(y = after_stat(density))) +
                      geom_density()+
                      scale_x_continuous(limits = densPlotXLim) +
                      theme_light() +
                      theme(
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        axis.title.x = element_blank(),
                        axis.title.y = element_blank())
  
  
  
  histDistPlot <- ggplotly(histDistPlot_GG) %>%
                  layout( margin = list(l=100,r=0,b=90,t=40,pad=0),
                         annotations= list(
                           list(
                             text = "Average Win Rate Differential",
                             x= 0.5,
                             y= -0.15,
                             xref = "paper",
                             yref = "paper",
                             showarrow =FALSE
                           ),
                           list(
                             text = "Density",
                             x= -0.13,
                             y= 0.57,
                             xref = "paper",
                             yref = "paper",
                             showarrow =FALSE
                           )))
  
  
  
  
  
  return (histDistPlot)
}#EOFn