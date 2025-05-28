

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
                              
  
  dmgWRDiffGGBase <- typeDF %>%
                   ggplot(aes(x= WR.Differential, y= DMG.Differential,
                              colour = Game.Result)) +
                   geom_point(size= 0.2, show.legend = FALSE) +
                   geom_hline(yintercept = 0.0, colour= "black",
                              alpha= 0.8, lwd = 0.5) +
                   geom_vline(xintercept = 0.0, colour= "black",
                              alpha= 0.8, lwd= 0.5) +
                   scale_x_continuous(limits = plotXLim) +
                   scale_y_continuous(limits = plotYLim) +
                   facet_wrap2(~ Game.Result, strip = stripPlot) +
                   scale_colour_manual(values = myDark2Plot) +
                   theme_light() +
                   labs(  title = 
                            paste0("WR versus Damage Differential for ", battleType),
                          x = "Win Rate Differential",
                          y = "Damage\n Differential") +
                   theme(
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.title.x = element_text(angle = 0),
                         axis.title.y = element_text(angle = 0)
                        )

  # dmgWRDiffPlot <-  ggplotly(dmgWRDiffGGBase) %>%
  #                   layout( showlegend = FALSE)

  
  return(dmgWRDiffGGBase)
  
}