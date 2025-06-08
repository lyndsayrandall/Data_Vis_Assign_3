# winLossByDayandHour.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the Win Loss and Draw by hour and day of week 
# of the collected data by battle or ship

plotByDay <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  # attrType <- "Submarine"
  # typeDF<- retTypeDF(gameData_human,attrType,"Ship.Type")
  # str(typeDF$gDay)
  count= 1
  byDay_plot_list <- list()
  for (attr in levels(typeDF$Game.Result)) { 
    tmp_df2 <- typeDF %>% filter(Game.Result ==attr) 
    
    countByDay <- tmp_df2 %>% 
      group_by(gDay,Game.Result) %>%
      mutate(Freq = n()) %>% 
      select(c("gDay","Freq","Game.Result")) %>%
      distinct() %>%
      ungroup()
    
    missDays <- setdiff(levels(typeDF$gDay),droplevels( countByDay$gDay))
    # Do this to add missing days for better plots
    for(missDay in missDays){
      print(missDay)
      countByDay <- add_row( countByDay, gDay = missDay, 
                             Freq = 0, Game.Result = attr)
    }
    #Refactor for print order
    countByDay$gDay <- factor( countByDay$gDay,
                               levels = levels(typeDF$gDay),)
    
    tmp_plot_2 <- countByDay %>%
      plot_ly(
        x = ~gDay,
        y = ~Freq,
        marker= list(color = myDark2[[count]]),
        type= "bar")  %>%
      layout(
        showlegend = FALSE
                                                  
      )
    
    byDay_plot_list[[count]] <- tmp_plot_2
    count <- count + 1
  }
  
  facet_byDay_plot <- subplot( byDay_plot_list, nrows = 1 ,
                                shareX = TRUE, shareY = TRUE)
  return(facet_byDay_plot)
}



plotByHour <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  
  # attrType <- "Random"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")
  
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  count= 1
  byHour_plot_list <- list()
  for (attr in levels(typeDF$Game.Result)) {
    
    tmp_df3 <- typeDF %>% filter(Game.Result == attr)
  
    countByHour <- tmp_df3 %>% 
      group_by(gHour,Game.Result) %>%
      mutate(Freq = n()) %>% 
      select(c("gHour","Freq","Game.Result")) %>%
      distinct() %>%
      ungroup()
    
   tmp_plot_3 <- countByHour %>%
      plot_ly(
        x = ~gHour,
        y = ~Freq,
        marker= list(color = myDark2[[count]]),
        type= "bar") %>%
     layout(
       xaxis = list(range = c(0,24)),
       showlegend = FALSE
     )
   
   byHour_plot_list[[count]] <- tmp_plot_3
   count <- count + 1
  }
  
  facet_byHour_plot <- subplot( byHour_plot_list, nrows = 1 ,
                                shareX = TRUE, shareY = TRUE)
  return(facet_byHour_plot)
}



