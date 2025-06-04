

slideTitle <- "Where the Wins are!!! World of Warships."
sideImage <- div( style = "height:10%;",
              tags$img(src = base64enc::dataURI(file = 'Images/game.ico',
                                                mime = 'image/vnd.microsoft.icon'),
                       alt='WOWS Icon SVG',
                       width = "100%"
              ))

tab_1_title <- function(queryTypeBattle,queryTypeShip,queryAttr){
  msg_tab_1<- paste0("Scatter, Histogram, Density: \n",
                      if_else(queryAttr == "Battle.Type",queryTypeBattle,
                              queryTypeShip),
                      " in ",if_else(queryAttr == "Battle.Type","Battle","Ship"))
  return(msg_tab_1)
}#EOFn

tab_2_title <- function(queryTypeBattle2,queryTypeShip2,queryAttr2){
  msg_tab_2<- paste0("Win Rate over Time: ",
                     if_else(queryAttr2 == "Battle.Type",queryTypeBattle2,
                             queryTypeShip2),
                     " in ",if_else(queryAttr2 == "Battle.Type","Battle","Ship"))
  return(msg_tab_2)
}#EOFn
listType <- as.list(gameType_human$Type)


attrRadioButton <- radioButtons("queryAttr", "Query Game Attribute",
                                choices = c("Battle" = "Battle.Type",
                                            "Ship" = "Ship.Type"),
                                inline = TRUE,
                                selected = "Battle.Type",
                                width = "100%")


selTypeBattle <- selectizeInput( "queryTypeBattle",
                                  label = "Query Battle Type",
                                  choices = c(unique(gameType_human$Type)),
                                  multiple = FALSE,
                                  selected = c("Random")
                                  )

selTypeShip <- selectizeInput( "queryTypeShip",
                                label = "Query Ship Type",
                                choices = c(unique(shipType$Ship.Type)),
                                multiple = FALSE,
                                selected = c("Destroyer")
                              )

attrRadioButton2 <- radioButtons("queryAttr2", "Query Game Attribute",
                                choices = c("Battle" = "Battle.Type",
                                            "Ship" = "Ship.Type"),
                                inline = TRUE,
                                selected = "Battle.Type",
                                width = "100%")


selTypeBattle2 <- selectizeInput( "queryTypeBattle2",
                                 label = "Query Battle Type",
                                 choices = c(unique(gameType_human$Type)),
                                 multiple = FALSE,
                                 selected = c("Random")
)

selTypeShip2 <- selectizeInput( "queryTypeShip2",
                               label = "Query Ship Type",
                               choices = c(unique(shipType$Ship.Type)),
                               multiple = FALSE,
                               selected = c("Destroyer")
)