# winLossByDayandHour.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the Win Loss and Draw by hour and day of week 
# of the collected data by battle or ship


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
    
    missHours <- setdiff(levels(typeDF$gHour),droplevels( countByHour$gHour))
    # Do this to add missing days for better plots
    for(missHour in missHours){
      print(missHour)
      countByHour <- add_row( countByHour, gHour = missHour, 
                             Freq = 0, Game.Result = attr)
    }
   
    #Refactor for print order
    countByHour$gHour <- factor( countByHour$gHour,
                               levels = levels(typeDF$gHour),
                               ordered = TRUE)
    str(countByHour$gHour)
    
   tmp_plot_3 <- countByHour %>%
      plot_ly(
        x = ~gHour,
        y = ~Freq,
        marker= list(color = myDark2[[count]]),
        hovertemplate = paste('<b><i>',attr,"</i></b>",
                              '<br><b>Hour(24hr)</b>: %{x}',
                              '<br><b>Count</b>: %{y} <br>',
                              '<extra></extra>'),        
        type= "bar") %>%
     layout(
       xaxis = list(range = c(0,24),
                    title = " "),
       yaxis = list(title = " ",
                    range = c(0,(max(countByHour$Freq)*1.1 + 1))),
       showlegend = FALSE) %>%
     layout(annotations = list(  text = levels(typeDF$Game.Result)[[count]],
                                 x= 0.05,
                                 y = 1.06,
                                 bgcolor = paste0(myDark2[[count]],"80"),
                                 bordercolor = "black",
                                 showarrow = FALSE,
                                 xref = "paper",
                                 yref = "paper"))
   
   byHour_plot_list[[count]] <- tmp_plot_3
   count <- count + 1
  }
  
  winLoss_commonY <-subplot(byHour_plot_list[[1]],byHour_plot_list[[2]],
                            nrows = 1, shareX =TRUE, shareY = TRUE) 
  
  facet_byHour_plot <- subplot( winLoss_commonY,byHour_plot_list[[3]], 
                                nrows = 1 , shareX = TRUE, shareY = FALSE,
                                widths = c(0.6667,0.3333)) %>%
                      layout(margin = list(l=85,r=0,b=90,t=40,pad=0),
                             annotations= list(
                               list(
                                 text = "Win, Loss, Draw Numbers by Hour of Day",
                                 x= 0.5,
                                 y= -0.2,
                                 xref = "paper",
                                 yref = "paper",
                                 showarrow =FALSE,
                                 font = list(size = 18)
                                 ),
                               list(
                                 text = "Total",
                                 x= -.05,
                                 y= 1.07,
                                 xref = "paper",
                                 yref = "paper",
                                 showarrow =FALSE,
                                 font = list(size = 16)
                               )
                             ))
  
  return(facet_byHour_plot)
}#EOFn

plotByDay <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  # attrType <- "Mode Shuffle"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")
  # str(typeDF$gDay)
  count= 1
  byDay_plot_list <- list()
  for (attr in levels(typeDF$Game.Result)) { 
    tmp_df2 <- typeDF %>% filter(Game.Result == attr) 
    
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
                               levels = levels(typeDF$gDay))
    str(countByDay$gDay)
    tmp_plot_2 <- countByDay %>%
      plot_ly(
        x = ~gDay,
        y = ~Freq,
        marker= list(color = myDark2[[count]]),
        hovertemplate = paste('<b><i>',attr,"</i></b>",
                              '<br><b>Day</b>: %{x}',
                              '<br><b>Count</b>: %{y} <br>',
                              '<extra></extra>'),
        type= "bar")  %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = " ",
                     range = c(0,(max(countByDay$Freq)*1.1 + 1))),
        showlegend = FALSE) %>%
      layout(annotations = list(  text = levels(typeDF$Game.Result)[[count]],
                                  x= 0.05,
                                  y = 1.06,
                                  bgcolor = paste0(myDark2[[count]],"80"),
                                  bordercolor = "black",
                                  showarrow = FALSE,
                                  xref = "paper",
                                  yref = "paper"))
    
    byDay_plot_list[[count]] <- tmp_plot_2
    count <- count + 1
  }
  winLoss_commonY_2 <-subplot(byDay_plot_list[[1]],byDay_plot_list[[2]],
                              nrows = 1, shareX =TRUE, shareY = TRUE)
  
  facet_byDay_plot <- subplot( winLoss_commonY_2, byDay_plot_list[[3]], 
                               nrows = 1 , shareX = TRUE, shareY = FALSE,
                               widths = c(0.6667,0.3333))%>%
                      layout(margin = list(l=85,r=0,b=90,t=40,pad=0),
                             annotations= list(
                               list(
                                 text = "Win, Loss, Draw Numbers by Day of Week",
                                 x= 0.5,
                                 y= -0.2,
                                 xref = "paper",
                                 yref = "paper",
                                 showarrow =FALSE,
                                 font = list(size = 18)
                               ),
                               list(
                                 text = "Total",
                                 x= -.05,
                                 y= 1.07,
                                 xref = "paper",
                                 yref = "paper",
                                 showarrow =FALSE,
                                 font = list(size = 16)
                               )
                             ))
  return(facet_byDay_plot)
}#EOFn


