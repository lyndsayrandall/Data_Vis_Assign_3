library(openxlsx)
library(readr)
library(dplyr)


slideTitle <- "Changing source"


gameType <- read.xlsx("../Data/BattleType.xlsx") %>%
            filter(!(BattleType == "No Spuds"))
colnames(gameType) <- c("Id", "Type")
