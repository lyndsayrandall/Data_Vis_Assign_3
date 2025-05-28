library(openxlsx)
library(readr)
library(dplyr)


slideTitle <- "Changing source"


gameType <- read.xlsx("../Data/BattleType.xlsx") %>%
            filter(!(BattleType == "No Spuds"))
colnames(gameType) <- c("Id", "Type")

gameType_human <- gameType %>%
                  filter(Type %in% humanOpposition) %>%
                  mutate(Type = case_when( Type %in% c(
                    "Arms Race","Convoy","Drigible Derby","Asymmetric lower")
                         ~ "Mode Shuffle",
                    TRUE ~ Type)) %>%
                  distinct(Type, .keep_all = TRUE)
                  
