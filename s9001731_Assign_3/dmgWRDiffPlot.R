

plotdmgWRDiff <- function(gamedata, battleType){
  
  typeDF <- retTypeDF(battleType)
  # typeDF<- retTypeDF("Random")
  # battleType <- "Random"
  rd_by_month <- retRdByMonth(typeDF)
 
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  dmgWRDiffGGBase <- typeDF %>%
                   ggplot(aes(x= WR.Differential, y= DMG.Differential,
                              colour = Game.Result)) +
                   geom_point(size= 0.1, show.legend = FALSE) +
                   facet_wrap(~ Game.Result) +
                   theme_light() +
                   theme(
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.border = element_blank(),
                         axis.ticks.x = element_blank(),
                         axis.ticks.y = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank()
                         )
  
  dmgWRDiffPlot <-  ggplotly(dmgWRDiffGGBase) %>%
                    layout( title = 
                              paste0("WR versus Damage Differential for ", battleType),
                            xaxis = list(title = "Win Rate\n Differential"),
                            yaxis = list(title = "Damage\n Differential"),
                            showlegend = FALSE) 
                    
  
  return(dmgWRDiffPlot)
  
}