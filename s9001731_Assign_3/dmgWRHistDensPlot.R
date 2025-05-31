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
  histDensVars <- factor(c("WR.Differential","DMG.Differential"),
                         levels = c("WR.Differential","DMG.Differential"),
                         ordered = TRUE
                         )
  histDens_plot_list <- list()
  count <- 1
  for (attr in histDensVars) {
    quartTypeDF <- quantile(typeDF[attr], prob = c(.25,.5,.75),
                            type = 1, na.rm = TRUE)
    
    attrDFIQR <- quartTypeDF[[3]] - quartTypeDF[[1]]
    
    densPlotXLim <- c((quartTypeDF[[1]] - 3*(attrDFIQR)),
                      (quartTypeDF[[3]] + 3*(attrDFIQR)))
    
    tmpPlot_gg_base <-  ggplot(typeDF,aes( x = .data[[attr]])) +
      geom_histogram(aes(y = after_stat(density))) +
      geom_density()+
      scale_x_continuous(limits = densPlotXLim) +
      theme_light() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
    
    
    tmpPlot_plotly <- ggplotly(tmpPlot_gg_base) %>%
      layout( margin = list(l=100,r=0,b=90,t=40,pad=0),
              annotations= list(
                list(
                  text = paste0("Average ",attr),
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
    histDens_plot_list[[count]] <- tmpPlot_plotly
    count <- count + 1
  }#EOFor
  
  histDistPlot <- subplot(histDens_plot_list , nrows =1,
                          shareX = FALSE, shareY = FALSE)
    

  return (histDistPlot)
}#EOFn