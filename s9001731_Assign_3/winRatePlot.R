# winRatePlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the Win Rate over recorded time 
# of the collected data by battle or ship

plotCumWin <- function(gamedata,queryType,queryAttr){
    
    typeDF <- retTypeDF(gamedata,queryType,queryAttr)
    
    # attrType <- "Light Cruiser"
    # typeDF<- retTypeDF(gameData_human,attrType,"Ship.Type")

   
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
    
    minDate <- min(gamedata$gDate)-months(1)
    maxDate <- max(gamedata$gDate)
    minWRPercent <- min(c(min(comb_by_Month$Win_Percent.x),
                          min(comb_by_Month$Win_Percent.y)))
    maxWRPercent <- max(c(max(comb_by_Month$Win_Percent.x),
                          max(comb_by_Month$Win_Percent.y)))
    # minDate <- min(gameData_human$gDate)
    # maxDate <- max(gameData_human$gDate)

    timeWRPlot <- comb_by_Month_frames %>%
                  plot_ly()%>%
                  add_trace(
                    
                    x= ~gDate,
                    y= ~Win_Percent.x,
                    frame = ~frame,
                    type= "scatter",
                    mode= "lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>All Games</b>: %{y:.2f}% <br>'
                                          ),
                    line = list(simplyfy = F),
                    showlegend = FALSE) %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Win_Percent.y,
                    frame = ~frame,
                    type= "scatter",
                    mode= "lines+markers",
                    hovertemplate = paste('<b>Date</b>: %{x|%d/%b/%Y}',
                                          '<br><b>',queryType,' Games</b>:', 
                                          '%{y:.2f}% <br>' ),
                    line = list(simplyfy = F),
                    showlegend = F)  %>%           
                  layout(xaxis = list(range = c((minDate),maxDate),
                                      title = ""),
                         yaxis = list(range = c(minWRPercent, 
                                                maxWRPercent),
                                      title = ""))
    return(timeWRPlot)
}

