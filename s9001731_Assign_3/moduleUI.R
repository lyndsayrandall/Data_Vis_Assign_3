

slideTitle <- "Where the Wins are!!! World of Warships."
sideImage <- div( style = "height:10%;",
              tags$img(src = base64enc::dataURI(file = 'Images/game.ico',
                                                mime = 'image/vnd.microsoft.icon'),
                       alt='WOWS Icon SVG',
                       width = "100%"
              ))

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