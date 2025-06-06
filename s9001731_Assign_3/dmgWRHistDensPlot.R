# dmgWRHistDensPlot.R
# Author: Mark Randall
# Date: 11 June 2025
# Plots the faceted Histogram and density plots for average WR Differential
# and average Damage Differential


plotdmgWRHistDens <- function(gamedata,queryType,queryAttr){
  
  typeDF <- retTypeDF(gamedata,queryType,queryAttr)
  # 
  # attrType <- "Random"
  # typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")

  if(plyr::empty(typeDF)){
    errPlot <- errorPlotFig()
    return(errPlot)
  }
  
  histDensVars <- factor(c("WR.Differential","DMG.Differential"),
                         levels = c("WR.Differential","DMG.Differential"),
                         ordered = TRUE
                         )
  
  
  histDens_plot_list <- list()
  count <- 1
  
  for (attr in histDensVars) {
    
    title_xaxis <- if_else(attr == "WR.Differential",
                           "Win Rate Differential Average",
                           "Damage Differential Average")
    print(title_xaxis)  
    quartTypeDF <- quantile(typeDF[attr], prob = c(.25,.5,.75),
                            type = 1, na.rm = TRUE)
    
    attrDFIQR <- quartTypeDF[[3]] - quartTypeDF[[1]]
    
    densPlotXLim <- c((quartTypeDF[[1]] - 3*(attrDFIQR)),
                      (quartTypeDF[[3]] + 3*(attrDFIQR)))
    plotBinWidth <- (attrDFIQR*2)/((dim(typeDF)[[1]]^(1/3)))
    
    tmpPlot_gg_base <-  ggplot(typeDF,aes( x = .data[[attr]])) +
      geom_histogram(aes(y = after_stat(density)), position = "identity",
                     binwidth = plotBinWidth, fill = mySpectral[[5]],
                     colour = mySpectral[[4]], stat = "bin") +
      geom_density(colour = mySpectral[[11]]) +
      scale_x_continuous(limits = densPlotXLim ,
                         expand = c(0,0)) +
      scale_y_continuous(limits = c(0.0,NA) ,
                         expand = c(0,0),
                         sec.axis = sec_axis(~.*dim(typeDF)[[1]])
      ) +
      theme_light() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(colour = "black") ,
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
    
    tmpBuild <- ggplot_build(tmpPlot_gg_base)
    
    tmpHist_df <- data.frame(xhis_plot = tmpBuild$data[[1]]$x ,
                             count_xplot = tmpBuild$data[[1]]$count)
    tmpDens_df <-data.frame(xdens_plot = tmpBuild$data[[2]]$x,
                            dens_xplot = tmpBuild$data[[2]]$density)
    max(tmpHist_df$count_xplot)
    
    tmpPlotly <- plot_ly() %>%
      add_trace(
        data = tmpHist_df,
        x = ~xhis_plot,
        y = ~count_xplot,
        hovertemplate = paste('<b>Differential</b>: %{x}',
                              '<br><b>Count</b>: %{y} <br>',
                              '<extra></extra>'),
        marker = list( color = mySpectral[[5]],
                       line = list(color = mySpectral[[11]] ,
                                   width = 0.75)),
        type = "bar"
      ) %>%
      add_trace(
        data = tmpDens_df,
        x = ~xdens_plot,
        y = ~dens_xplot,
        hovertemplate = paste('<b>Differential</b>: %{x}',
                              '<br><b>Density</b>: %{y} <br>',
                              '<extra></extra>'),
        yaxis = "y2",
        line = list(color = mySpectral[[11]]),
        type = "scatter",
        mode = "lines"
      ) %>%
      layout(
        margin = list(l = 50,r = 50,b = 90,t = 45,pad = 0),
        annotations= list(
          list(
            text = title_xaxis,
            x= 0.5,
            y= -0.11,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "center",
            showarrow =FALSE
          ),
          list(
            text = "Density",
            x= 1.00,
            y= 1.00,
            xref = "paper",
            yref = "paper",
            showarrow =FALSE
          ),
          list(
            text = "Count",
            x= 0.00,
            y= 1.00,
            xref = "paper",
            yref = "paper",
            showarrow =FALSE
          ),
          list(
            text= TeX("$Bin\\;Width\\;Used:\\\\Freedman-Diaconis'\\;\\;bin_{size} = \\frac{(2*IQR)}{\\sqrt[3]{n}}$") ,
            x= 0.5,
            y= -0.30,
            xref = "paper",
            yref = "paper",
            showarrow = FALSE)),
        
        xaxis = list(title = "",
                     range = c(min(tmpHist_df$xhis_plot),max(tmpHist_df$xhis_plot))),
        yaxis = list(title = ""),
        yaxis2 = list(title = "",
                      overlaying = "y",
                      side = "right",
                      range = c(0 ,max(tmpDens_df$dens_xplot))),
        showlegend = FALSE
      ) %>%
      config(mathjax = "cdn")
    
    histDens_plot_list[[count]] <- tmpPlotly
    
    count <- count + 1
  }#EOFor
    

  return (histDens_plot_list)
}#EOFn







#  tear down ggplot to build in plotly.
# histDens_plot_list <- list()
# count <- 1
# for (attr in histDensVars) {
#   
#   title_xaxis <- if_else(attr == "WR.Differential",
#                          "Win Rate Differential Average",
#                          "Damage Differential Average")
#   print(title_xaxis)  
#   quartTypeDF <- quantile(typeDF[attr], prob = c(.25,.5,.75),
#                           type = 1, na.rm = TRUE)
#   
#   attrDFIQR <- quartTypeDF[[3]] - quartTypeDF[[1]]
#   
#   densPlotXLim <- c((quartTypeDF[[1]] - 3*(attrDFIQR)),
#                     (quartTypeDF[[3]] + 3*(attrDFIQR)))
#   plotBinWidth <- (attrDFIQR*2)/((dim(typeDF)[[1]]^(1/3)))
#   
#   tmpPlot_gg_base <-  ggplot(typeDF,aes( x = .data[[attr]])) +
#     geom_histogram(aes(y = after_stat(density)), position = "identity",
#                    binwidth = plotBinWidth, fill = mySpectral[[5]],
#                    colour = mySpectral[[4]]) +
#     geom_density(colour = mySpectral[[11]]) +
#     scale_x_continuous(limits = densPlotXLim ,
#                        expand = c(0,0)) +
#     scale_y_continuous(limits = c(0.0,NA) ,
#                        expand = c(0,0),
#                        sec.axis = sec_axis(~.*dim(typeDF)[[1]])
#     ) +
#     theme_light() +
#     theme(
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.border = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       axis.line = element_line(colour = "black") ,
#       plot.margin = margin(t = 0, r = 0, b = 0, l = 0))
#   
#   
#   tmpPlot_plotly <- ggplotly(tmpPlot_gg_base) %>%
#     layout( margin = list(l = 50,r = 10,b = 90,t = 40,pad = 0),
#             annotations= list(
#               list(
#                 text = title_xaxis,
#                 x= 0.4,
#                 y= -0.14,
#                 xref = "paper",
#                 yref = "paper",
#                 xanchor = "center",
#                 yanchor = "center",
#                 showarrow =FALSE
#               ),
#               list(
#                 text = "Density",
#                 x= 0.05,
#                 y= 1.05,
#                 xref = "paper",
#                 yref = "paper",
#                 showarrow =FALSE
#               )),
#             xaxis = list(zeroline = FALSE,
#                          range = c(densPlotXLim[[1]],
#                                    densPlotXLim[[2]]) ) ,
#             yaxis = list(zeroline = FALSE )
#     ) #end layout
#   
#   histDens_plot_list[[count]] <- tmpPlot_plotly
#   count <- count + 1
# }#EOFor
# 
# histDistPlot <- subplot(histDens_plot_list, nrows = 1,
#                         shareX = FALSE, shareY = FALSE,
#                         margin = c(0.1,0.1,0.00,0.00)) %>%
#   layout(annotations = list(
#     text= TeX("$Bin\\;Width\\;Used:\\;Freedman-Diaconis'\\;
#                                                  \\;bin_{size} = \\frac{(2*IQR)}{\\sqrt[3]{n}}$") ,
#     x= 0.5,
#     y= -0.30,
#     xref = "paper",
#     yref = "paper",
#     showarrow = FALSE
#   ))

