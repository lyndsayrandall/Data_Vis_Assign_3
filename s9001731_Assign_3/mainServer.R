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
source("dmgWRHistDensPlot.R")

options(scipen=10000)

gameData <- read.xlsx("../Data/gameData.xlsx")  %>%
  mutate(Timestamp = convertToDateTime(Date+Time),
         gDate = convertToDate(Date),
         gYear = lubridate::year(Timestamp),
         gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
         gHour = lubridate::hour(Timestamp)) %>%
  filter(!(is.na(WR.Differential) | Game.Result == "" ))
colnames(gameData)[5] <- "ShipId"

shipData <- read.xlsx("../Data/Ship Data.xlsx")
shipType <- read.xlsx("../Data/Ship Type.xlsx")

gameData %<>% left_join(shipData ,
                        by= c("ShipId" = "ShipId"))

gameData %<>% left_join(shipType ,
                        by= c("Ship.Type" = "Ship.Type"))

humanOpposition <- c("Random", "Ranked", "Clan", "Brawl Clan", "Arms Race", 
                     "Dirigible Derby", "Mode Shuffle", "Convoy", "Brawl","Asymmetric lower")
gameData_human <- gameData %>%
  filter(Battle.Type %in% humanOpposition) %>%
  mutate(Battle.Type = case_when( Battle.Type %in% c(
    "Arms Race","Convoy","Drigible Derby","Asymmetric lower")
    ~ "Mode Shuffle",
    TRUE ~ Battle.Type))

last(gameData$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gameData %>% 
           filter(gYear < 2021 | gYear > 2025)









