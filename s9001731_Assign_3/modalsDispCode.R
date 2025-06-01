

dispRefModal <- function(input,value) {
observeEvent(input[[value]], {
  showModal(modalDialog(
    title = HTML("<b><u>References:</u></b>"),
    size = "l",
    HTML(paste("Beadell, S. J. L. R. N. (1941). View from HMS HERMIONE of HMS LEGION moving alongside the damaged and 
                  listing HMS ARK ROYAL in order to take off survivors. In HMS_Ark_Royal_sinking_2.jpg (Ed.), jpg (Vol. 761x 
                  571px). Imperial War Museums(collection no. 4700-01): Imperial War Museums.",
                " ",
                "Wargaming.net. (2012-2025, 2025). World of Warships. Wargaming.net. 
                    Retrieved 10 May from https://worldofwarships.asia/en/content/game/",
                " ",
                "Perry Swift. (2025). Potato Alert, A statistics companion app for World Of Warships. razaqq. 
                    Retrieved 10 May from https://github.com/razaqq/PotatoAlert?tab=readme-ov-file",
                " ",
          sep = "<br>")),
    footer = tagList(modalButton("Return"))
  ))})    
}

dispAccessModal <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "Title",
      size = "l",
      div( style='overflow:scroll',
          tags$img(src= base64enc::dataURI(file="Images/WOWS spudsRecord.png",
                                           mime="image/png"), 
                   alt="Relationships for WOWS Access Data Base",
                   width = "150%",
                   height = "120%")),
      footer = tagList(modalButton("Return"))
    ))})   
}#EOFStructure


dispAboutModal <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "About",
      
      fluidRow(
        column(1, 
                 div( style = "height:10%;",
                   tags$img(src = base64enc::dataURI(file = 'Images/game.ico',
                                              mime = 'image/vnd.microsoft.icon'),
                     alt='WOWS Icon SVG',
                     width = "100%"
                 ))),
        column(11,
               HTML(paste("World of Warships is a 'free' multiplayer on line game developed by Wargaming. 
               The game was annouced as World of Battleships to complement two other games in the wargaming stable. 
               This annoucement occured 16 August 2011. The game conducted a closed beta testing 12 March 2015 
               with a name change of World of Warships(2012). Open beta testing commenced 2 July 2015. 
               The microsoft windows official launch was 17 September 2015 and in the Microsoft Store and Steam 
               15 November 2015.",
               "The Game genre is considered massively multiplayer online(MMO)/Vehicular Combat Game/
               third person shooter(TPS). It has games clients for the following
               platforms Microsoft Windows, macOS (discontinued), iOS, Android, PlayStation 4, PlayStation 5, 
               Xbox One, Xbox Series X/S.",
               "'Players control warships of choice and can battle other random players on the server, 
               play cooperative battles against bots, or participate in an advanced player versus environment 
               (PvE) battle mode. For the most skilled players, two seasonal competitive modes are also available.'",
                          " ",
                          sep = "<br>")),
      )),
      fluidRow(
        column(1, 
               div( style = "height:10%;",
                    tags$img(src = base64enc::dataURI(file = 'Images/potato.svg',
                                              mime = 'image/svg+xml'),
                             alt='Potato Icon SVG',
                             width = "100%"
                    ))),
        column(11,
               HTML(paste("Beadell, S. J. L. R. N. (1941). View from HMS HERMIONE of HMS LEGION moving alongside the damaged and 
                  listing HMS ARK ROYAL in order to take off survivors. In HMS_Ark_Royal_sinking_2.jpg (Ed.), jpg (Vol. 761x 
                  571px). Imperial War Museums(collection no. 4700-01): Imperial War Museums.",
                          " ",
                          "Wargaming.net. (2012-2025, 2025). World of Warships. Wargaming.net. 
                    Retrieved 10 May from https://worldofwarships.asia/en/content/game/",
                          " ",
                          "Perry Swift. (2025). Potato Alert, A statistics companion app for World Of Warships. razaqq. 
                    Retrieved 10 May from https://github.com/razaqq/PotatoAlert?tab=readme-ov-file",
                          " ",
                          sep = "<br>")),
               )),
      footer = tagList(modalButton("Return")),
         ))})  
}#EOFnAbout
  