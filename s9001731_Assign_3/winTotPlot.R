# winTotalPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the Game and Win  over recorded time 
# of the collected data by battle or ship

plotWinTot <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  # attrType <- "Frigate"
  # typeDF<- retTypeDF(gameData_human,attrType,"Ship.Type")
  
  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  rd_by_month <- retRdByMonth(typeDF)
  rd_by_month_All <- retRdByMonth(gamedata)
  #rd_by_month_All <- retRdByMonth(gameData_human)
  comb_by_Month <-  rd_by_month_All %>%
                    left_join(rd_by_month,
                              by = c("gDate","gMonth","gYear")) %>%
    mutate_if(is.numeric,coalesce,0)
 
  comb_by_Month_frames <- comb_by_Month %>%
    accumulate_by(~gDate) %>%
    mutate(frame = as.numeric(frame)) 
    
  
  minDate <- min(gamedata$gDate) -months(1)
  maxDate <- max(gamedata$gDate)
  
   # minDate <- min(gameData_human$gDate)
   # maxDate <- max(gameData_human$gDate)
  
  slider_lab_list <- list(unique(comb_by_Month_frames$gDate))
  
  print(paste("type", typeof(slider_lab_list)))
  winTotPlot <- comb_by_Month_frames %>%
                  plot_ly() %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Win.x,
                    frame = ~ frame,
                    type= "scatter",
                    mode ="lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>Total Wins</b>: %{y} <br>',
                                          '<extra></extra>'),
                    line = list(simplyfy = F)) %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Total.x,
                    frame = ~ frame,
                    type= "scatter",
                    mode ="lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>Total Games</b>: %{y} <br>',
                                          '<extra></extra>'),
                    line = list(simplyfy = F)) %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Win.y,
                    frame = ~ frame,
                    type= "scatter",
                    mode= "lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>',queryType,' Wins</b>:', 
                                          '%{y} <br><extra></extra>' ),
                    line = list(simplyfy = F)) %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Total.y,
                    frame = ~ frame,
                    type= "scatter",
                    mode ="lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>',queryType,'Total</b>:', 
                                          '%{y} <br><extra></extra>' ),
                    line = list(simplyfy = F)) %>% 
                layout(margin = list(l = 100,r = 0,b = 90,t = 40,pad = 0),
                       annotations= list(
                         list(
                           text = paste0("Games: Total and ", queryType) ,
                           x= 0.45,
                           y= -0.14,
                           xref = "paper",
                           yref = "paper",
                           xanchor = "center",
                           yanchor = "center",
                           showarrow =FALSE ,
                           font = list(size = 18)
                         ),
                         list(
                           text = "Games Played",
                           x= 0.0,
                           y= 1.05,
                           xref = "paper",
                           yref = "paper",
                           showarrow =FALSE,
                           font = list(size = 15)
                         )),
                       xaxis = list(range= c(minDate,maxDate),
                                    title = ""),
                       yaxis = list( title = ""),
                       showlegend = FALSE,
                       hovermode = "x unified"
                       ) %>%
                animation_slider(hide = TRUE) %>%
                animation_button(x = 1.1, xanchor = "right",
                                 y = -0.01, yanchor = "bottom") 
  
  return(winTotPlot)
  
}