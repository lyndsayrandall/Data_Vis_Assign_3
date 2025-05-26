


plotCumWin <- function(gamedata,battleType ){
    
    typeDF <- retTypeDF(battleType)

   
    rd_by_month <- retRdByMonth(typeDF)
    
    if(plyr::empty(rd_by_month)){
      errPlot <- errorPlotFig()
      return(errPlot)
    }
    cat("plotCumWin")
    str(rd_by_month)
    
    minDate <- min(rd_by_month$gDate)
    maxDate <- max(rd_by_month$gDate)

    rd_Frames <- rd_by_month %>%
    accumulate_by(~gDate) %>%
    mutate(frame = as.numeric(frame))
    str(rd_Frames$frame)

    cumWinPlot <-  rd_Frames %>%
                   plot_ly() %>%
                   add_trace(
                     x= ~gDate,
                     y= ~Win_Percent,
                     frame = ~frame,
                     type= "scatter",
                     mode= "lines+markers",
                     line = list(simplyfy = F),
                     showlegend = FALSE) %>%
                   layout(xaxis = list(range= c(minDate,maxDate)))
    
    return(cumWinPlot)
}