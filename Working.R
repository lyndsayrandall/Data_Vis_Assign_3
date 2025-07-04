library(openxlsx)
library(readr)
library(dplyr)
library(readxl)
library(datetimeutils)
library(lubridate)
library(data.table)
library(ggplot2)
library(plotly)
library(tidyr)
library(magrittr)


gamedata <- read.xlsx("s9001731_Assign_3/Data/GameData.xlsx")  %>%
            mutate(Timestamp = convertToDateTime(Date+Time),
                   gDate = convertToDate(Date),
                   gYear = lubridate::year(Timestamp),
                   gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
                   gHour = lubridate::hour(Timestamp),
                   gDay = lubridate::wday(Timestamp, week_start = 7, label= TRUE, abbr= TRUE)) %>%
            filter(!(is.na(WR.Differential) | Game.Result == "" ))
colnames(gamedata)[5] <- "ShipId"
shipData <- read.xlsx("s9001731_Assign_3/Data/Ship Data.xlsx")
shipType <- read.xlsx("s9001731_Assign_3/Data/Ship Type.xlsx")
gamedata %<>% left_join(shipData ,
                        by= c("ShipId" = "ShipId"))

gamedata %<>% left_join(shipType ,
                        by= c("Ship.Type" = "Ship.Type"))

humanOpposition <- c("Random", "Ranked", "Clan", "Brawl Clan", "Arms Race", 
                     "Dirigible Derby", "Mode Shuffle", "Convoy", "Brawl","Asymmetric lower")
gameData_human <- gamedata %>%
  filter(Battle.Type %in% humanOpposition) %>%
  mutate(Battle.Type = case_when( Battle.Type %in% c(
    "Arms Race","Convoy","Drigible Derby","Asymmetric lower")
    ~ "Mode Shuffle",
    TRUE ~ Battle.Type))

last(gamedata$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gamedata %>% 
           filter(gYear < 2021 | gYear > 2025)

str(gamedata)

outDate <- gamedata %>%
        filter(Game.Result == "")
# Modified from https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}


random_data <- gamedata %>% 
  filter(Battle.Type == "Random")

rd_by_month <- random_data %>%
               group_by(gYear, gMonth) %>%
               summarise(Win = sum(Game.Result == "Win"),
                         Loss = sum(Game.Result == "Loss" |
                                    Game.Result == "Draw"),
                         Total = Win + Loss,
                         Win_Percent = (Win/Total)*100) %>%
               ungroup() %>%
               filter(!(is.na(gYear))) %>%
               mutate(gDate = lubridate::ym(paste0(gYear,gMonth)))





# %>%
#                mutate(gYear = year(tDate),
#                       gMonth = month(tDate))
# %>%
#                pivot_longer( cols = c(3:5),
#                              names_to = "Result",
#                              values_to = "Number")
               

str(rd_by_month)


test <- rd_by_month %>%
        plot_ly() %>%
            add_trace(
                x= ~gMonth,
                y= ~Win,
                frame = ~by,
                type= "scatter",
                mode= "lines",
                showlegend = FALSE) %>%
            add_trace(
              x= ~gMonth,
              y= ~Total,
              frame = ~by,
              type= "scatter",
              mode= "lines",
              showlegend = FALSE) %>%           
        layout(xaxis = list(range= c(0,12))) 

minDate <- min(rd_by_month$gDate)
maxDate <- max(rd_by_month$gDate)

rd_Frames <- rd_by_month %>% 
             accumulate_by(~gDate) %>%
             mutate(frame = as.numeric(frame))
str(rd_Frames$frame)

test3 <- rd_Frames %>%
        plot_ly() %>%
        add_trace(
          x= ~gDate,
          y= ~Win_Percent,
          frame = ~frame,
          type= "scatter",
          mode= "lines", 
          line = list(simplyfy = F),
          showlegend = FALSE) %>%           
          layout(xaxis = list(range= c(minDate,maxDate)))
  

test2 <- rd_by_month %>%
        ggplot(aes(x= gMonth,y= Win)) +
        geom_point()

summary(random_data$Game.Result)
unique(random_data$Game.Result)
noWinData <- random_data %>%
  filter(Game.Result == "")

ranData2 <- random_data %>%
  filter(Game.Result != "") %>%
  mutate(WRGP = case_when((WR.Differential <= -10) ~"<= -10",
                          (-10 < WR.Differential &  WR.Differential <= -9) ~"-10/-9",
                          (-9 < WR.Differential &  WR.Differential <= -8) ~"-9/-8",
                          (-8 < WR.Differential &  WR.Differential <= -7) ~"-8/-7",
                          (-7 < WR.Differential &  WR.Differential <= -6) ~"-7/-6",
                          (-6 < WR.Differential &  WR.Differential <= -5) ~"-6/-5",
                          (-5 < WR.Differential &  WR.Differential <= -4) ~"-5/-4",
                          (-4 < WR.Differential &  WR.Differential <= -3) ~"-4/-3",
                          (-3 < WR.Differential &  WR.Differential <= -2) ~"-3/-2",
                          (-2 < WR.Differential &  WR.Differential <= -1) ~"-2/-1",
                          (-1 < WR.Differential &  WR.Differential <= 0) ~"-1/0",
                          (0 < WR.Differential &  WR.Differential <= 1) ~"0/1",
                          (1 < WR.Differential &  WR.Differential <= 2) ~"1/2",
                          (2 < WR.Differential &  WR.Differential <=3) ~"2/3",
                          (3 < WR.Differential &  WR.Differential <= 4) ~"3/4",
                          (4 < WR.Differential &  WR.Differential <= 5) ~"4/5",
                          (5< WR.Differential &  WR.Differential <= 6) ~"5/6",
                          (6 < WR.Differential &  WR.Differential <= 7) ~"6/7",
                          (7 < WR.Differential &  WR.Differential <=8) ~"7/8",
                          (8 < WR.Differential &  WR.Differential <= 9) ~"8/9",
                          (9 < WR.Differential &  WR.Differential <= 10) ~"9/10",
                          TRUE ~ ">10"),
         WRGP = factor(WRGP, ordered =TRUE, 
                       levels= c("<= -10", "-10/-9", "-9/-8", "-8/-7", "-7/-6", "-6/-5",
                                 "-5/-4", "-4/-3", "-3/-2", "-2/-1", "-1/0", "0/1", "1/2",
                                 "2/3", "3/4", "4/5", "5/6", "6/7", "7/8", "8/9", "9/10", ">10")))
str(ranData2)
colTest <-"magenta"
histRanData <- ggplot(ranData2) +
  geom_bar(aes(WRGP),fill = colTest)

histRanData 

slideTitle <- "Old Faithful Geyser Data"

ranData2Gp <- ranData2 %>%
  group_by(WRGP) %>%
  mutate(Total = n(),
         Total_Win = sum(Game.Result == "Win"),
         Total_Loss = sum(Game.Result == "Loss" | Game.Result == "Draw")) %>%
  ungroup() %>%
  select(c(WRGP,Total,Total_Win,Total_Loss)) %>%
  arrange(WRGP) %>% 
  distinct() %>%
  mutate(Prob_Win = (Total_Win/Total),
         Prob_Loss = (Total_Loss/Total),
         Err_Chk = (Prob_Win + Prob_Loss))


count(ranData2, WRGP,Game.Result)

ggplot(ranData2Gp,aes(x= WRGP, y = Prob_Win)) +
  geom_point() +
  geom_line()


# dmgWRDiffGGBase <- typeDF %>%
#                  ggplot(aes(x= WR.Differential, y= DMG.Differential,
#                             colour = Game.Result)) +
#                  geom_point(size= 0.2, show.legend = FALSE) +
#                  geom_hline(yintercept = 0.0, colour= "black",
#                             alpha= 0.8, lwd = 0.5) +
#                  geom_vline(xintercept = 0.0, colour= "black",
#                             alpha= 0.8, lwd= 0.5) +
#                  scale_x_continuous(limits = plotXLim) +
#                  scale_y_continuous(limits = plotYLim) +
#                  facet_wrap2(~ Game.Result, strip = stripPlot) +
#                  scale_colour_manual(values = myDark2Plot) +
#                  theme_light() +
#                  labs(  title = 
#                           paste0("WR versus Damage Differential for ", attrType),
#                         x = "Win Rate Differential",
#                         y = "Damage\n Differential") +
#                  theme(
#                        panel.grid.major = element_blank(), 
#                        panel.grid.minor = element_blank(),
#                        panel.border = element_blank(),
#                        axis.ticks.x = element_blank(),
#                        axis.ticks.y = element_blank(),
#                        axis.title.x = element_text(angle = 0),
#                        axis.title.y = element_text(angle = 0)
#                       )

attrType <- "Random"
typeDF<- retTypeDF(gameData_human,attrType,"Battle.Type")

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
  attr= "WR.Differential"
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
                   colour = mySpectral[[4]]) +
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
      type = "bar"
    ) %>%
    add_trace(
      data = tmpDens_df,
      x = ~xdens_plot,
      y = ~dens_xplot,
      yaxis = "y2",
      type = "scatter",
      mode = "lines"
    ) %>%
    layout(
      margin = list(l = 50,r = 50,b = 90,t = 40,pad = 0),
      annotations= list(
                    list(
                      text = title_xaxis,
                      x= 0.5,
                      y= -0.15,
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
                      text= TeX("$Bin\\;Width\\;Used:\\;Freedman-Diaconis'\\;
                                   \\;bin_{size} = \\frac{(2*IQR)}{\\sqrt[3]{n}}$") ,
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
    )
  
  
  # tmpPlot_plotly <- ggplotly(tmpPlot_gg_base) %>%
  #   layout( margin = list(l = 50,r = 10,b = 90,t = 40,pad = 0),
  #           annotations= list(
  #             list(
  #               text = title_xaxis,
  #               x= 0.4,
  #               y= -0.14,
  #               xref = "paper",
  #               yref = "paper",
  #               xanchor = "center",
  #               yanchor = "center",
  #               showarrow =FALSE
  #             ),
  #             list(
  #               text = "Density",
  #               x= 0.05,
  #               y= 1.05,
  #               xref = "paper",
  #               yref = "paper",
  #               showarrow =FALSE
  #             )),
  #           xaxis = list(zeroline = FALSE,
  #                        range = c(densPlotXLim[[1]],
  #                                  densPlotXLim[[2]]) ) ,
  #           yaxis = list(zeroline = FALSE )
  #   ) #end layout
  
  histDens_plot_list[[count]] <- tmpPlotly
  count <- count + 1
}#EOFor

sub_sub_plot <- subplot(histDens_plot_list[[1]],nrows =1, shareX =T, shareY = F)
sub_sub_plot2 <- subplot(histDens_plot_list[[2]],nrows = 1, shareX =T, shareY = F)

histDistPlot <- subplot(sub_sub_plot, sub_sub_plot2, plotly_empty(),nrows = 1,
                        shareX = FALSE, shareY = FALSE,
                        margin = c(0.1,0.1,0.00,0.00)) %>%
  layout(annotations = list(
    text= TeX("$Bin\\;Width\\;Used:\\;Freedman-Diaconis'\\;\\;bin_{size} = \\frac{(2*IQR)}{\\sqrt[3]{n}}$") ,
    x= 0.5,
    y= -0.30,
    xref = "paper",
    yref = "paper",
    showarrow = FALSE
  ))

new_df <- gameData_human %>% 
          select(c("Battle.Type","ShipId","WR.Differential", "gDay",
                                      "DMG.Differential","gHour", "Game.Result"))
                             
tpm <- GGally::ggpairs(new_df, aes(colour = Game.Result))

ggplotly(tpm)