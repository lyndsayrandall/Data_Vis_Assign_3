# dmgWRDiffPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the facetted Scatter plot  average WR Differential
# versus average Damage Differential

plotdmgWRDiff <- function(gamedata, queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  # attrType <- "Random"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")

 
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
    quad1 <- dim(tmp_df %>% filter(WR.Differential > 0 & DMG.Differential > 0 ))[[1]]
    quad2 <- dim(tmp_df %>% filter(WR.Differential <= 0 & DMG.Differential > 0 ))[[1]]
    quad3 <- dim(tmp_df %>% filter(WR.Differential <= 0 & DMG.Differential <= 0 ))[[1]]
    quad4 <- dim(tmp_df %>% filter(WR.Differential > 0 & DMG.Differential <= 0 ))[[1]]

    #print(paste("quad 1", quad1,quad2 ,quad3, quad4))
   
    tmp_plot <- plot_ly( tmp_df,
                         x = ~WR.Differential,
                         y = ~DMG.Differential,
                         type = "scatter",
                         mode = "markers",
                         hovertemplate = paste('<b><i>Differential</i></b>',
                                               '<br><b>Win Rate</b>: %{x}',
                                               '<br><b>Damage</b>: %{y} <br>',
                                               '<extra></extra>'),
                         marker= list(color = myDark2[[count]],
                                      size = 4)) %>%
                layout(showlegend = FALSE,
                       xaxis = list(title = "",
                                    range = plotXLim),
                       yaxis = list(title = "",
                                    range = plotYLim)) %>%
                layout(annotations = list(
                  list( text = levels(typeDF$Game.Result)[[count]],
                               x= 0.5,
                               y = 1.06,
                               bgcolor = paste0(myDark2[[count]],"80"),
                               bordercolor = "black",
                               showarrow = FALSE,
                               xref = "paper",
                               yref = "paper"),
                 list( text =   quad1,
                                x= 0.9,
                                y= 1.0,
                                showarrow = FALSE,
                                font = list(color = myBlues[[8]]),
                                bgcolor = myBlues[[2]],
                                bordercolor = myBlues[[8]],
                                xref = "paper",
                                yref= "paper",
                                hovertext=paste("Games in",
                                                "<br>Quadrant 1")
                               ),
                  list( text =  quad2,
                                x= 0.0,
                                y= 1.0,
                                showarrow = FALSE,
                                font = list(color = myBuPu[[6]]),
                                bgcolor = myBuPu[[2]],
                                bordercolor = myBuPu[[8]],
                                xref = "paper",
                                yref= "paper",
                                hovertext=paste("Games in",
                                                "<br>Quadrant 2")
                               ),
                  list( text =  quad3,
                                x= 0.0,
                                y= 0.0,
                                showarrow = FALSE,
                                font = list(color = myYlOrRd[[8]]),
                                bgcolor = myYlOrRd[[2]],
                                bordercolor = myYlOrRd[[8]],
                                xref = "paper",
                                yref= "paper",
                                hovertext=paste("Games in",
                                                "<br>Quadrant 3")
                                ),
                  list( text =  quad4,
                                x= 0.9,
                                y= 0.0,
                                showarrow = FALSE,
                                font = list(color = myBuPu[[6]]),
                                bgcolor = myBuPu[[2]],
                                bordercolor = myBuPu[[8]],
                                xref = "paper",
                                yref= "paper",
                                hovertext=paste("Games in",
                                                "<br>Quadrant 3")
                        )
                 ))
                                
                  
                  
                       
                
    
    plotWRDmgDiff_plot_list[[count]] <- tmp_plot
    count= count+1
  }#EOFor
  facet_plot <- subplot(plotWRDmgDiff_plot_list, nrows = 1, 
                        shareX = TRUE, shareY = TRUE  ) %>%
                layout(margin = list(l=85,r=0,b=90,t=40,pad=0),
                       annotations= list(
                              list(
                                text = "Average Win Rate Differential",
                                x= 0.5,
                                y= -0.15,
                                xref = "paper",
                                yref = "paper",
                                showarrow =FALSE,
                                font = list(size = 18)
                              ),
                              list(
                                text = "Average Damage Differential",
                                x= -.035,
                                y= 1.08,
                                xref = "paper",
                                yref = "paper",
                                showarrow =FALSE,
                                font = list(size = 18)
                              )
                         
                       ))

  
  return(facet_plot)
  
}#EOFn


