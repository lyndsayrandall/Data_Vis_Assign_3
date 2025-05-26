library(openxlsx)
library(readr)
library(dplyr)
library(readxl)
library(datetimeutils)
library(lubridate)
library(data.table)
library(ggplot2)

library(tidyr)
library(magrittr)

source("winTotPlot.R")
source("winRatePlot.R")

gamedata <- read.xlsx("../Data/GameData.xlsx")  %>%
            mutate(Timestamp = convertToDateTime(Date+Time),
                   gDate = convertToDate(Date),
                   gYear = lubridate::year(Timestamp),
                   gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
                   gHour = lubridate::hour(Timestamp)) %>%
            filter(!(is.na(WR.Differential) |
                       Game.Result == "" |
                       Battle.Type == "No Spuds") )

last(gamedata$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gamedata %>% 
           filter(gYear < 2021 | gYear > 2025)

outDate <- gamedata %>%
        filter(Game.Result == "")




# summary(random_data$Game.Result)
# unique(random_data$Game.Result)
# noWinData <- random_data %>%
#   filter(Game.Result == "")
# 
# ranData2 <- random_data %>%
#   filter(Game.Result != "") %>%
#   mutate(WRGP = case_when((WR.Differential <= -10) ~"<= -10",
#                           (-10 < WR.Differential &  WR.Differential <= -9) ~"-10/-9",
#                           (-9 < WR.Differential &  WR.Differential <= -8) ~"-9/-8",
#                           (-8 < WR.Differential &  WR.Differential <= -7) ~"-8/-7",
#                           (-7 < WR.Differential &  WR.Differential <= -6) ~"-7/-6",
#                           (-6 < WR.Differential &  WR.Differential <= -5) ~"-6/-5",
#                           (-5 < WR.Differential &  WR.Differential <= -4) ~"-5/-4",
#                           (-4 < WR.Differential &  WR.Differential <= -3) ~"-4/-3",
#                           (-3 < WR.Differential &  WR.Differential <= -2) ~"-3/-2",
#                           (-2 < WR.Differential &  WR.Differential <= -1) ~"-2/-1",
#                           (-1 < WR.Differential &  WR.Differential <= 0) ~"-1/0",
#                           (0 < WR.Differential &  WR.Differential <= 1) ~"0/1",
#                           (1 < WR.Differential &  WR.Differential <= 2) ~"1/2",
#                           (2 < WR.Differential &  WR.Differential <=3) ~"2/3",
#                           (3 < WR.Differential &  WR.Differential <= 4) ~"3/4",
#                           (4 < WR.Differential &  WR.Differential <= 5) ~"4/5",
#                           (5< WR.Differential &  WR.Differential <= 6) ~"5/6",
#                           (6 < WR.Differential &  WR.Differential <= 7) ~"6/7",
#                           (7 < WR.Differential &  WR.Differential <=8) ~"7/8",
#                           (8 < WR.Differential &  WR.Differential <= 9) ~"8/9",
#                           (9 < WR.Differential &  WR.Differential <= 10) ~"9/10",
#                           TRUE ~ ">10"),
#          WRGP = factor(WRGP, ordered =TRUE, 
#                        levels= c("<= -10", "-10/-9", "-9/-8", "-8/-7", "-7/-6", "-6/-5",
#                                  "-5/-4", "-4/-3", "-3/-2", "-2/-1", "-1/0", "0/1", "1/2",
#                                  "2/3", "3/4", "4/5", "5/6", "6/7", "7/8", "8/9", "9/10", ">10")))
# str(ranData2)
# colTest <-"magenta"
# histRanData <- ggplot(ranData2) +
#   geom_bar(aes(WRGP),fill = colTest)







