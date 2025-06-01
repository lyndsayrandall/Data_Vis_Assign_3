# dmgWRDiffPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the facetted Scatter plot  average WR Differential
# versus average Damage Differential

plotdmgWRDiff <- function(gamedata, attrType){
  
  typeDF <- retTypeDF(gamedata,attrType)
  attrType <- "Random"
  typeDF<- retTypeDF(gameData_human,attrType)

 
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  plotXLim <- c((min(typeDF$WR.Differential) - 1),
                (max(typeDF$WR.Differential) + 1))
  plotYLim <- c((min(typeDF$DMG.Differential) - 1),
                 (max(typeDF$DMG.Differential) + 1))
  
  numOfResults <- length(unique(typeDF$Game.Result))

  if (numOfResults > 2){
    stripPlot <- ggh4x::strip_themed(background_x = 
                  elem_list_rect(fill = myDark2[1:numOfResults]))
    myDark2Plot <- myDark2[1:numOfResults] 
  } else {
    stripPlot <- ggh4x::strip_themed(background_x = 
                  elem_list_rect(fill = myDark2[2:(numOfResults+1)]))
    myDark2Plot <- myDark2[2:(numOfResults+1)]
  }
                              
  
  count= 1
  plotWRDmgDiff_plot_list <- list()
  for (attr in levels(typeDF$Game.Result)) {
    tmp_df <- typeDF %>% filter(Game.Result == attr)
    tmp_plot <- plot_ly( tmp_df,
                         x = ~WR.Differential,
                         y = ~DMG.Differential,
                         type = "scatter",
                         mode = "markers",
                         marker= list(color = myDark2[[count]],
                                      size = 4)) %>%
                layout(showlegend = FALSE,
                       xaxis = list(title = "",
                                    range = plotXLim),
                       yaxis = list(title = "",
                                    range = plotYLim)) %>%
                layout(annotations = list(text = levels(typeDF$Game.Result)[[count]],
                                          x= 0.5,
                                          y = 1.06,
                                          bgcolor = paste0(myDark2[[count]],"80"),
                                          bordercolor = "black",
                                          showarrow = FALSE,
                                          xref = "paper",
                                          yref = "paper"))
    
    plotWRDmgDiff_plot_list[[count]] <- tmp_plot
    count= count+1
  }#EOFor
  facet_plot <- subplot(plotWRDmgDiff_plot_list, nrows = 1, 
                        shareX = TRUE, shareY = TRUE  ) %>%
                layout(margin = list(l=100,r=0,b=90,t=40,pad=0),
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
                                text = "Average\nDamage\nDifferential",
                                x= -0.13,
                                y= 0.57,
                                xref = "paper",
                                yref = "paper",
                                showarrow =FALSE
                              )
                         
                       ))

  
  return(facet_plot)
  
}#EOFn


