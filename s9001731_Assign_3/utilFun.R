
# Taken from https://plotly.com/r/cumulative-animations/
accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}#EOFn

retTypeDF <- function(battleType){
  tmp_data <- gamedata %>% 
    filter(Battle.Type == battleType)
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


