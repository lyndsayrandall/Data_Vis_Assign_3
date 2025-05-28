# mainServer.R
# Author: Mark Randall
# Date: 11 June 2025
# Loads r libraries not required in the engine file.
# Reads data file and filters for relevant information.
# Data cleaned through dbBeaver sqlite and microsoft access
# Loads specific modules as source. 

library(openxlsx)
library(readr)
library(dplyr)
library(readxl)
library(datetimeutils)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggh4x)
library(RColorBrewer)
library(tidyr)
library(magrittr)

source("winTotPlot.R")
source("winRatePlot.R")
source("errorPlotsMessages.R")
source("dmgWRDiffPlot.R")

gameData <- read.xlsx("../Data/GameData.xlsx")  %>%
            mutate(Timestamp = convertToDateTime(Date+Time),
                   gDate = convertToDate(Date),
                   gYear = lubridate::year(Timestamp),
                   gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
                   gHour = lubridate::hour(Timestamp)) %>%
            filter(!(is.na(WR.Differential) |
                       Game.Result == "" |
                       Battle.Type == "No Spuds") )

humanOpposition <- c("Random", "Ranked", "Clan", "Brawl Clan", "Arms Race", 
                     "Dirigible Derby", "Mode Shuffle", "Convoy", "Brawl","Asymmetric lower")
gameData_human <- gameData %>%
                  filter(Battle.Type %in% humanOpposition) %>%
                  mutate(Battle.Type = case_when( Battle.Type %in% c(
                    "Arms Race","Convoy","Drigible Derby","Asymmetric lower")
                        ~ "Mode Shuffle",
                    TRUE ~ Battle.Type))
unique(gameData_human$Battle.Type)
last(gameData$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gameData %>% 
           filter(gYear < 2021 | gYear > 2025)









