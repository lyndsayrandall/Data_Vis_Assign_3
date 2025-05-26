

plotWinTot <- function(gamedata,battleType ) {
  
  
  typeDF <- retTypeDF(battleType)

  
  rd_by_month <- retRdByMonth(typeDF)
  
  if(plyr::empty(rd_by_month)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  rd_Frames <- rd_by_month %>%
    accumulate_by(~gDate) %>%
    mutate(frame = as.numeric(frame))
  
  str(rd_Frames$frame)
  
  minDate <- min(rd_by_month$gDate)
  maxDate <- max(rd_by_month$gDate)
  
  winTotPlot <- rd_Frames %>%
                plot_ly() %>%
                add_trace(
                  x= ~ gDate,
                  y= ~ Win,
                  frame = ~ frame,
                  type= "scatter",
                  mode= "lines+markers",
                  line = list(simplyfy = F),
                  showlegend = FALSE) %>%
                add_trace(
                  x= ~ gDate,
                  y= ~ Total,
                  frame = ~ frame,
                  type= "scatter",
                  mode ="lines+markers",
                  line = list(simplyfy = F),
                  showlegend = FALSE) %>%           
                layout(xaxis = list(range= c(minDate,maxDate))) 
  
  return(winTotPlot)
  
}