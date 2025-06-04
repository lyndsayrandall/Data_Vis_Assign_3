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
    minWR <- min(c(min(comb_by_Month$Win_Percent.x),
                          min(comb_by_Month$Win_Percent.y)))
    minWRPercent <- if_else(minWR < 5.0, 0.0, (minWR-5) )
    maxWR <- max(c(max(comb_by_Month$Win_Percent.x),
                          max(comb_by_Month$Win_Percent.y)))
    maxWRPercent <- if_else(maxWR > 95.0,100.0, (maxWR+5) )
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
                  layout( margin = list(l = 100,r = 0,b = 90,t = 40,pad = 0),
                          annotations= list(
                            list(
                              text = paste0("Win Rates Total and ", queryType) ,
                              x= 0.45,
                              y= -0.14,
                              xref = "paper",
                              yref = "paper",
                              xanchor = "center",
                              yanchor = "center",
                              showarrow =FALSE ,
                              font = list(size = 15)
                            ),
                            list(
                              text = "Percentage",
                              x= -0.13,
                              y= 1.05,
                              xref = "paper",
                              yref = "paper",
                              showarrow =FALSE,
                              font = list(size = 15)
                            )),
                    
                           xaxis = list(range = c(minDate,maxDate),
                                      title = ""
                                      ),
                            yaxis = list(range = c(minWRPercent, 
                                                maxWRPercent),
                                      title = "")) %>%
                  animation_slider(hide = TRUE) %>%
                  animation_button(x = 1.1, xanchor = "right",
                                 y = -0.01, yanchor = "bottom") 
    
    return(timeWRPlot)
}

