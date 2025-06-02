# dmgWRHistDensPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the faceted Histogram and density plots for average WR Differential
# and average Damage Differential


plotdmgWRHistDens <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  
  # attrType <- "Destroyer"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")
  
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
    plotBinWidth <- (attrDFIQR*2)/((dim(typeDF)[[1]]^(1/3)))
    
    tmpPlot_gg_base <-  ggplot(typeDF,aes( x = .data[[attr]])) +
      geom_histogram(aes(y = after_stat(density)), position = "identity",
                     binwidth = plotBinWidth) +
      geom_density()+
      scale_x_continuous(limits = densPlotXLim ,
                         expand = c(0,0)) +
      scale_y_continuous(limits = c(0.0,NA) ,
                         expand = c(0,0),
                         sec.axis = sec_axis(~.*dim(typeDF)[[1]])
                         ) +
      theme_light() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black") ,
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
    
    
    tmpPlot_plotly <- ggplotly(tmpPlot_gg_base) %>%
      layout( margin = list(l = 100,r = 0,b = 90,t = 40,pad = 0),
              annotations= list(
                list(
                  text = paste0("Average ",attr),
                  x= 0.4,
                  y= -0.135,
                  xref = "paper",
                  yref = "paper",
                  xanchor = "center",
                  yanchor = "center",
                  showarrow =FALSE
                ),
                list(
                  text = "Density",
                  x= -0.2,
                  y= 0.95,
                  xref = "paper",
                  yref = "paper",
                  showarrow =FALSE
                )),
                xaxis = list(zeroline = FALSE,
                             range = c(densPlotXLim[[1]],
                                       densPlotXLim[[2]]) ) ,
               yaxis = list(zeroline = FALSE )
              ) #end layout
    
    histDens_plot_list[[count]] <- tmpPlot_plotly
    count <- count + 1
  }#EOFor
  
  histDistPlot <- subplot(histDens_plot_list , nrows =1,
                          shareX = TRUE, shareY = FALSE,
                          margin = c(0.1,0.05,0.00,0.00)) %>%
                  layout(annotations = list(
                                       text= TeX("$Bin\\;Width\\;Used:\\;Freedman-Diaconis'\\;\\;bin_{size} = \\frac{(2*IQR)}{\\sqrt[3]{n}}$") ,
                                       x= 0.5,
                                       y= -0.30,
                                       xref = "paper",
                                       yref = "paper",
                                       showarrow = FALSE
                  ))
    

  return (histDistPlot)
}#EOFn