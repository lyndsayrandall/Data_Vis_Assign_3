

plotdmgWRDiff <- function(gamedata, battleType){
  
  typeDF <- retTypeDF(gamedata,battleType)
  # typeDF<- retTypeDF("Brawl")
  # battleType <- "Random"

 
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
                              
  
  # dmgWRDiffGGBase <- typeDF %>%
  #                  ggplot(aes(x= WR.Differential, y= DMG.Differential,
  #                             colour = Game.Result)) +
  #                  geom_point(size= 0.2, show.legend = FALSE) +
  #                  geom_hline(yintercept = 0.0, colour= "black",
  #                             alpha= 0.8, lwd = 0.5) +
  #                  geom_vline(xintercept = 0.0, colour= "black",
  #                             alpha= 0.8, lwd= 0.5) +
  #                  scale_x_continuous(limits = plotXLim) +
  #                  scale_y_continuous(limits = plotYLim) +
  #                  facet_wrap2(~ Game.Result, strip = stripPlot) +
  #                  scale_colour_manual(values = myDark2Plot) +
  #                  theme_light() +
  #                  labs(  title = 
  #                           paste0("WR versus Damage Differential for ", battleType),
  #                         x = "Win Rate Differential",
  #                         y = "Damage\n Differential") +
  #                  theme(
  #                        panel.grid.major = element_blank(), 
  #                        panel.grid.minor = element_blank(),
  #                        panel.border = element_blank(),
  #                        axis.ticks.x = element_blank(),
  #                        axis.ticks.y = element_blank(),
  #                        axis.title.x = element_text(angle = 0),
  #                        axis.title.y = element_text(angle = 0)
  #                       )

  count= 1
  WRDMGDiff_plot_list <- list()
  for (attr in levels(typeDF$Game.Result)){
    tmp_df<- typeDF %>% filter(Game.Result == attr)
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
                                          showarrow = FALSE,
                                          xref = "paper",
                                          yref = "paper"))
    
    WRDMGDiff_plot_list[[count]] <- tmp_plot
    count= count+1
  }
  facet_plot <- subplot(WRDMGDiff_plot_list, nrows = 1, 
                        shareX = TRUE, shareY = TRUE  ) %>%
                layout(margin = list(l=95,r=0,b=90,t=40,pad=0),
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
                                text = "Average\n Damage\n Differential",
                                x= -0.155,
                                y= 0.5,
                                xref = "paper",
                                yref = "paper",
                                showarrow =FALSE
                              )
                         
                       ))

  
  return(facet_plot)
  
}