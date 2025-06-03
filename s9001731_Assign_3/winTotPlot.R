# winTotalPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the Game and Win  over recorded time 
# of the collected data by battle or ship

plotWinTot <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  # attrType <- "Random"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")
  
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
                layout(xaxis = list(range= c(minDate,maxDate),
                                    title = ""),
                       yaxis = list( title = ""),
                       showlegend = FALSE,
                       hovermode = "x unified"
                       ) %>%
                animation_slider(
                      currentvalue = list(prefix = "Date"))
  
  return(winTotPlot)
  
}