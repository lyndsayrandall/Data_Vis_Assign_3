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
library(RColorBrewer)

myBuPu = c(brewer.pal(name="BuPu",n=9))
myPuBu = c(brewer.pal(name="PuBu",n=9))
mySpectral = c(brewer.pal(name="Spectral",n=11))
myPuRd = c(brewer.pal(name="PuRd",n=9))
myYlGn = c(brewer.pal(name="YlGn",n=9))
myPaired = c(brewer.pal(name="Paired",n=12))
myDark2 = c(brewer.pal(name="Dark2",n=8))
myGreens = c(brewer.pal(name="Greens",n=9))
myBlues =  c(brewer.pal(name="Blues",n=9))



gameData <- read.xlsx("s9001731_Assign_3/Data/GameData.xlsx")  %>%
  mutate(Timestamp = convertToDateTime(Date+Time),
         gDate = convertToDate(Date),
         gYear = lubridate::year(Timestamp),
         gMonth = lubridate::month(Timestamp, label =TRUE,abbr = TRUE ),
         gHour = lubridate::hour(Timestamp)) %>%
  filter(!(is.na(WR.Differential) | Game.Result == "" ))
colnames(gameData)[5] <- "ShipId"

shipData <- read.xlsx("s9001731_Assign_3/Data/Ship Data.xlsx")
shipType <- read.xlsx("s9001731_Assign_3/Data/Ship Type.xlsx")
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

chkAllyDmgDiff <- gameData %>%
                  filter(Allies.DMG.Ave <= 20000)

chkOppDmgDif <- gameData %>%
  filter((Opp.DMG.Ave < 20000) &
          (Battle.Type %in% c("Random","Ranked","Clan","Brawl Clan","Brawl",
                              "Convoy","Arms Race","Mode Shuffle"))    )

gameType <- read.xlsx("s9001731_Assign_3/Data/BattleType.xlsx")
colnames(gameType) <- c("Id", "Type")

last(gameData$gDate)

last(gameData$gDate)
# Find dates outside reference to examine original data base.
# Then return to access to repair.
outDate <- gameData %>% 
  filter(gYear < 2021 | gYear > 2025)

str(gameData)

outDate <- gameData %>%
  filter(Game.Result == "")

selBattleType <- function(){
  print(gameType)
  selTypeAns <- readline("Please Enter Id number\n")
  retType <- gameType[selTypeAns,2]
  
  return(retType)
}

#battleType <- selBattleType()
battleType <- "Brawl Clan"




retTypeDF <- function(battleType){
  tmp_data <- gameData %>% 
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
  mutate(gDate = lubridate::ym(paste0(gYear,gMonth))) %>%
  distinct()

rd_by_month_All <- gameData_human %>%
  group_by(gYear, gMonth) %>%
  summarise(Win = sum(Game.Result == "Win"),
            Loss = sum(Game.Result == "Loss" |
                         Game.Result == "Draw"),
            Total = Win + Loss,
            Win_Percent = if_else(Total > 1,(Win/Total)*100, NA)) %>%
  ungroup() %>%
  filter(!(is.na(gYear))) %>%
  mutate(gDate = lubridate::ym(paste0(gYear,gMonth))) %>%
  distinct()

testComb <- rd_by_month_All %>%
            left_join(rd_by_month,
                      by = c("gDate","gMonth","gYear")) %>%
            mutate_if(is.numeric,coalesce,0)

minDate <- min(rd_by_month_All$gDate)- months(1)
maxDate <- max(rd_by_month_All$gDate)

rd_Frames <- rd_by_month %>% 
  accumulate_by(~gDate) %>%
  mutate(frame = as.numeric(frame))
str(rd_Frames$frame)

rd_Frames_All <- rd_by_month_All %>%
  accumulate_by(~gDate) %>%
  mutate(frame = as.numeric(frame))
test_frames_all <- testComb %>%
  accumulate_by(~gDate) %>%
  mutate(frame = as.numeric(frame))

chk1 <- test_frames_all[,c(7,12)]

steps_slider_list <- list()

for (posn in 1:dim(test_frames_all)[[1]]) {
  
  steps_slider_list[[posn]] <- list(args= list(name = test_frames_all[posn,7] ))
  
}



cumWinPlot <- test_frames_all %>%
  plot_ly() %>%
  add_trace(
    x= ~gDate,
    y= ~Win_Percent.y,
    frame = ~frame,
    type= "scatter",
    mode= "lines+markers", 
    line = list(simplyfy = F),
    showlegend = FALSE) %>%           
  layout(xaxis = list(range= c(minDate,maxDate)))

cumWinPlot_All <- test_frames_all %>%
                  plot_ly() %>%
                  add_trace(
                    x= ~gDate,
                    y= ~Win_Percent.x,
                    frame = ~frame,
                    type= "scatter",
                    mode= "lines+markers",
                    line = list(simplyfy = F),
                    showlegend = FALSE)  %>%           
                    layout(xaxis = list(range= c(minDate,maxDate)))

testSlider <- test_frames_all %>%
              plot_ly()%>%
              add_trace(
                x= ~gDate,
                y= ~Win_Percent.x,
                frame = ~frame,
                type= "scatter",
                mode= "lines+markers",
                line = list(simplyfy = F),
                showlegend = TRUE) %>%
              add_trace(
                x= ~gDate,
                y= ~Win_Percent.y,
                frame = ~frame,
                type= "scatter",
                mode= "lines+markers",
                line = list(simplyfy = F),
                showlegend = T)  %>%           
                layout(xaxis = list(range= c(minDate,maxDate))) %>%
              animation_slider(
                step = steps_slider_list
              )

# p <- plot_ly(
#   data = mtcars,
#   x = ~hp,
#   y = ~drat,
#   type = "scatter",
#   mode = "markers",
#   color = ~ as.factor(cyl)
# )
# 
# pb <- plotly_build(p)


# pb<- plotly_build(testSlider)
# 
# traceNames <- list("Total Win Percent ",
#                    paste(battleType,"Win Percent"))
# for(i in 1:length(traceNames)){
#   pb$x$data[[i]]$name <- traceNames[[pb$x$data[[i]]$name]]
# }
# pb$x$data[[i]]$name <-traceNames[[1]]


timeWRPlot <- subplot(cumWinPlot,cumWinPlot_All , nrows =2,
                      shareX = TRUE, shareY = TRUE,
                      margin = c(0.1,0.05,0.1,0.00)) %>%
  layout(xaxis = list(range= c(minDate,maxDate)),
         yaxis = list(range= c(0,100)))

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


library(reticulate)
use_condaenv("base")

pt <- import("plotly")
cols1 <- 3L
t1 <-pt$express$scatter(typeDF, x = "WR.Differential", y = "DMG.Differential",
                        facet_col = "Game.Result", color = "Game.Result",
                        facet_col_wrap = 3L,
                        labels = dict(WR.Differential = "Average Win Rate Differential",
                                      DMG.Differential = "Average Damge Differential"))
t1 <- t1$update_yaxes()

t2 <- t1$show()
group_labels <- c('distplot')
t3 <- pt$express$histogram(typeDF, x = "WR.Differential", nbins = 1000L)

t4 <- t3$show()

df1 <- diamonds

dens <- with(diamonds, tapply(price, INDEX = cut, density))

df2 <- data.frame(
  x = unlist(lapply(dens, "[[", "x")),
  y = unlist(lapply(dens, "[[", "y")),
  cut = rep(names(dens), each = length(dens[[1]]$x))
)

dens1 <- with(typeDF, tapply(WR.Differential, INDEX = Battle.Type, density))

df3 <- data.frame(
  x = unlist(lapply(dens1, "[[", "x")),
  y = unlist(lapply(dens1, "[[", "y")),
  Battle.Type = rep(names(dens1), each = length(dens1[[1]]$x))
)

fig <- plot_ly(df3, x = ~x, y = ~y, color = ~Battle.Type) 
fig <- fig %>% add_lines()

fig

# histplot <- plot_ly(x= typeDF$WR.Differential, 
#                     type = "histogram",
#                     histnorm = "density",
#                     yaxis = "y1",
#                     xaxis = "x1")
# 
# dens2 <- density(typeDF$WR.Differential)
# histplot %>%
#   add_lines(x= dens2$x,
#             y= dens2$y ,
#             type = "scatter",
#             mode="lines",
#             yaxis = "y2",
#             xaxis = "x2",
#             line = list(color = "red")) %>%
#             layout(yaxis2 = list(overlaying = "y1",
#                                  side = "right",
#                                  yref = "y"
#                                  ),
#                    xaxis2 = list(overlaying = "x1",
#                                 side = "top")
#                    )
# 
# str(dens2)
# str(typeDF$WR.Differential)


histDistPlot_GG <- ggplot(typeDF,aes( x= WR.Differential)) +
                   geom_histogram(aes(y = after_stat(density))) +
                   geom_density()
ggplotly(histDistPlot_GG)


histDensVars <- factor(c("WR.Differential","DMG.Differential"),
                       levels = c("WR.Differential","DMG.Differential"),
                       ordered = TRUE)

for(attr in histDensVars){
  quartTypeDF <- quantile(typeDF[attr], prob= c(.25,.5,.75),
                          type = 1, na.rm =TRUE)
  
  attrDFIQR <- quartTypeDF[[3]] - quartTypeDF[[1]]
  
  densPlotXLim <- c((quartTypeDF[[1]] - 3*(attrDFIQR)),
                    (quartTypeDF[[3]] + 3*(attrDFIQR)))
 
  histDistPlot_GG <-  ggplot(typeDF,aes( x = .data[[attr]])) +
    geom_histogram(aes(y = after_stat(density)), position = "identity") +
    geom_density()+
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
      axis.line = element_line(colour = "black"))


  histDistPlot <- ggplotly(histDistPlot_GG) %>%
    layout( margin = list(l=100,r=10,b=90,t=40,pad=0),
            annotations= list(
              list(
                text = paste0("Average ",attr),
                x= 0.5,
                y= -0.095,
                xref = "paper",
                yref = "paper",
                showarrow =FALSE
              ),
              list(
                text = "Density",
                x= -0.2,
                y= 0.57,
                xref = "paper",
                yref = "paper",
                showarrow =FALSE
              )))
}

retTypeDF2 <- function(gamedata, queryType, queryAttr){
  
  tmp_data <- gamedata %>% 
    filter(get(queryAttr) == queryType)
}

queryType <- "Battleship"
queryAttr <- "Ship.Type"
typeDFComb<- retTypeDF2(gameData_human,queryType,queryAttr)

plotXLim <- c((min(typeDFComb$WR.Differential) - 1),
              (max(typeDFComb$WR.Differential) + 1))
plotYLim <- c((min(typeDFComb$DMG.Differential) - 1),
              (max(typeDFComb$DMG.Differential) + 1))

numOfResults <- length(unique(typeDFComb$Game.Result))

# if (numOfResults > 2){
#   stripPlot <- ggh4x::strip_themed(background_x = 
#                                      elem_list_rect(fill = myDark2[1:numOfResults]))
#   myDark2Plot <- myDark2[1:numOfResults] 
# } else {
#   stripPlot <- ggh4x::strip_themed(background_x = 
#                                      elem_list_rect(fill = myDark2[2:(numOfResults+1)]))
#   myDark2Plot <- myDark2[2:(numOfResults+1)]
# }


count= 1
plotWRDmgDiff_plot_list2 <- list()
for (attr in levels(typeDFComb$Game.Result)) {
  tmp_df <- typeDFComb %>% filter(Game.Result == attr)
  tmp_plot <- plot_ly( tmp_df,
                       x = ~WR.Differential,
                       y = ~DMG.Differential,
                       type = "scatter",
                       mode = "markers",
                       marker= list(color = myDark2[[count]],
                                    size = 4)) %>%
    layout(showlegend = FALSE,
           xaxis = list(title = "",
                        range = plotXLim),
           yaxis = list(title = "",
                        range = plotYLim)) %>%
    layout(annotations = list(text = levels(typeDF$Game.Result)[[count]],
                              x= 0.5,
                              y = 1.06,
                              bgcolor = paste0(myDark2[[count]],"80"),
                              bordercolor = "black",
                              showarrow = FALSE,
                              xref = "paper",
                              yref = "paper"))
  
  plotWRDmgDiff_plot_list[[count]] <- tmp_plot
  count= count+1
}#EOFor
facet_plot <- subplot(plotWRDmgDiff_plot_list2, nrows = 1, 
                      shareX = TRUE, shareY = TRUE  ) %>%
  layout(margin = list(l=100,r=0,b=90,t=40,pad=0),
         annotations= list(
           list(
             text = "Average Win Rate Differential",
             x= 0.5,
             y= -0.15,
             xref = "paper",
             yref = "paper",
             showarrow =FALSE
           ),
           list(
             text = "Average\nDamage\nDifferential",
             x= -0.13,
             y= 0.57,
             xref = "paper",
             yref = "paper",
             showarrow =FALSE
           )
           
         ))

countByHour <- typeDF %>% 
               group_by(gHour) %>%
               mutate(Freq = n()) %>% 
               select(c("gHour","Freq")) %>%
               distinct() %>%
               ungroup()

hourPlot <- countByHour %>%
            plot_ly(
              x = ~gHour,
              y = ~Freq,
             
              type= "bar")


# library(ggpubr)
# library(cowplot)
# 
# 
# phist <- gghistogram(
#              typeDF, x = "WR.Differential")
# 
# pdensity <- ggdensity(
#   typeDF, x = "WR.Differential") +
#   
#   scale_y_continuous(expand = expansion(mult = c(0, 0.05)), position = "right")  +
#   theme_half_open(11, rel_small = 1) +
#   rremove("x.axis")+
#   rremove("xlab") +
#   rremove("x.text") +
#   rremove("x.ticks") +
#   rremove("legend")
# aligned_plots <- align_plots(phist, pdensity, align="hv")
# ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])