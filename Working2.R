# Working2.R
# Author: Mark Randall
# Date: 11 June 2025


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


gamedata <- read.xlsx("Data/GameData.xlsx")  %>%
  mutate(Timestamp = convertToDateTime(Date+Time),
         gDate = convertToDate(Date),
         gYear = lubridate::year(Timestamp),
         gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
         gHour = lubridate::hour(Timestamp)) %>%
  filter(!(is.na(WR.Differential) | Game.Result == "" ))

chkAllyDmgDiff <- gamedata %>%
                  filter(Allies.DMG.Ave <= 20000)

chkOppDmgDif <- gamedata %>%
  filter((Opp.DMG.Ave < 20000) &
          (Battle.Type %in% c("Random","Ranked","Clan","Brawl Clan","Brawl",
                              "Convoy","Arms Race","Mode Shuffle"))    )

gameType <- read.xlsx("Data/BattleType.xlsx")
colnames(gameType) <- c("Id", "Type")

last(gamedata$gDate)

last(gamedata$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gamedata %>% 
  filter(gYear < 2021 | gYear > 2025)

str(gamedata)

outDate <- gamedata %>%
  filter(Game.Result == "")

selBattleType <- function(){
  print(gameType)
  selTypeAns <- readline("Please Enter Id number\n")
  retType <- gameType[selTypeAns,2]
  
  return(retType)
}

#battleType <- selBattleType()
battleType <- "Random"




retTypeDF <- function(battleType){
  tmp_data <- gamedata %>% 
    filter(Battle.Type == battleType)
  if(plyr::empty(tmp_data)){
    return("Empty Data Frame")
    
  } else {
    return(tmp_data)
  }
}

typeDF <- retTypeDF(battleType)

# Taken from https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

rd_by_month <- typeDF %>%
  group_by(gYear, gMonth) %>%
  summarise(Win = sum(Game.Result == "Win"),
            Loss = sum(Game.Result == "Loss" |
                         Game.Result == "Draw"),
            Total = Win + Loss,
            Win_Percent = if_else(Total > 1,(Win/Total)*100, NA)) %>%
  ungroup() %>%
  filter(!(is.na(gYear))) %>%
  mutate(gDate = lubridate::ym(paste0(gYear,gMonth)))


minDate <- min(rd_by_month$gDate)
maxDate <- max(rd_by_month$gDate)

rd_Frames <- rd_by_month %>% 
  accumulate_by(~gDate) %>%
  mutate(frame = as.numeric(frame))
str(rd_Frames$frame)

cumWinPlot <- rd_Frames %>%
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

wrDiffData <- typeDF %>%
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
str(wrDiffData)

# colTest <-"magenta"
# histRanData <- ggplot(wrDiffData) +
#   geom_bar(aes(WRGP),fill = colTest)
# 
# wrDiffDataPlot <- wrDiffData %>% 
#                   # group_by(WRGP) %>%
#                   # summarise(Frequency = n()) %>%
#                   # ungroup() %>% 
#                   plot_ly( x = ~WRGP,
#                           
#                            type = "histogram")

wrBinSize <- IQR(typeDF$WR.Differential)/((dim(typeDF)[[1]]^(1/3)))

testGG <- ggplot(typeDF,aes(x = WR.Differential)) + 
          geom_histogram(aes(y = after_stat(density)), binwidth = wrBinSize) +
          geom_density(colour = "red")

summary(typeDF$WR.Differential)

testGGPlotly <- ggplotly(testGG)


testGGPlot5 <- typeDF %>%
               ggplot(aes(x= WR.Differential, y= DMG.Differential)) +
               geom_point() +
               facet_grid( ~ Game.Result)

typeDF%>%
  group_by(Game.Result) %>%
  do(p=plot_ly(., x = ~WR.Differential, y = ~DMG.Differential, color = ~Game.Result, type = "scatter")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)
View(iris)

iris%>%
  group_by(Species) %>%
  do(p=plot_ly(., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter")) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)

iris%>%
  group_by(Species) %>%
  group_map(~ plot_ly(data=., x = ~Sepal.Length, y = ~Sepal.Width, color = ~Species, type = "scatter", mode="markers"), .keep=TRUE) %>%
  subplot(nrows = 1, shareX = TRUE, shareY=TRUE)

typeDF%>%
  group_by(Game.Result) %>%
  group_map(~ plot_ly(data=., x = ~WR.Differential, y = ~DMG.Differential, color = ~Game.Result, type = "scatter", mode="markers"), .keep=TRUE) %>%
  subplot(nrows = 1, shareX = TRUE, shareY=TRUE)


count= 1
WRDMGDiff_plot_list <- list()
for (attr in unique(typeDF$Game.Result)){
  tmp_df<- typeDF %>% filter(Game.Result == attr)
  tmp_plot <- plot_ly( tmp_df,
                       x = ~WR.Differential,
                       y = ~DMG.Differential,
                       type = "scatter",
                       mode = "markers")

  WRDMGDiff_plot_list[[count]] <- tmp_plot
  count= count+1
}
facet_plot <- subplot(WRDMGDiff_plot_list, nrows = 1, shareX = TRUE, shareY = TRUE  )