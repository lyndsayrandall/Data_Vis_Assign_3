# utilFun.R
# Author: Mark Randall
# Date: 11 June 2025
# Utility Functions and Constants


# Taken from https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}#EOFn

retTypeDF <- function(gamedata, queryType, queryAttr){
  
  tmp_data <- gamedata %>% 
    filter(get(queryAttr) == queryType)
}

retRdByMonth <- function(typeDF) {
  
  tmp2_DF <-  typeDF %>%
              group_by(gYear,gMonth) %>%
              summarise(Win = sum(Game.Result == "Win"),
                        Loss = sum(Game.Result == "Loss" |
                                     Game.Result == "Draw"),
                        Total = Win + Loss,
                        Win_Percent = if_else(Total > 1,(Win/Total)*100, NA)) %>%
              ungroup() %>%
              filter(!(is.na(gYear))) %>%
              mutate(gDate = lubridate::ym(paste0(gYear,gMonth)))
  return(tmp2_DF)
}#EOFn

# utilFun.R
# COLOUR PALETTES
# Define a range of colour palettes to be used in the
# application


myBuPu = c(brewer.pal(name="BuPu",n=9))
myPuBu = c(brewer.pal(name="PuBu",n=9))
mySpectral = c(brewer.pal(name="Spectral",n=11))
myPuRd = c(brewer.pal(name="PuRd",n=9))
myYlGn = c(brewer.pal(name="YlGn",n=9))
myPaired = c(brewer.pal(name="Paired",n=12))
myDark2 = c(brewer.pal(name="Dark2",n=8))
myGreens = c(brewer.pal(name="Greens",n=9))
myBlues =  c(brewer.pal(name="Blues",n=9))
myYlOrRd = c(brewer.pal(name="YlOrRd",n=9))

# Function to convert hex to rgbs with opacity 
hex2rgba <- function(hexColour, aHex) {
  
  tmpConv <- col2rgb(hexCol)
  if (aHex > 1){
    aHex = 1
  } else if (aHex < 0){
    aHex = 0
  } else {
    aHex = aHex
  }
  tmpRGBA <- paste0("rgba(",tmpConv[[1]],", ",tmpConv[[2]]
                    ,", ",tmpConv[[3]],", ",aHex,")")
  
  return(tmpRGBA)
  
}
