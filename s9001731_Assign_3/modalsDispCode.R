

dispRefModal <- function(input,value) {
observeEvent(input[[value]], {
  showModal(modalDialog(
    title = HTML("<b><u>References:</u></b>"),
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
      div( style='overflow:scroll',
          tags$img(src= base64enc::dataURI(file="Images/relnDBImg.png",
                                           mime="image/png"), 
                   alt="Relationships for WOWS Access Data Base",
                   width = "150%",
                   height = "120%")),
      footer = tagList(modalButton("Return"))
    ))})   
}


dispAboutModal <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "About",
      div( style='overflow:scroll',
           tags$img(src = base64enc::dataURI(file = 'Images/potato.svg',
                                             mime = 'image/svg+xml'),
                    alt='Potato Icon SVG'
                    )),
      div( style='overflow:scroll',
           tags$img(src = base64enc::dataURI(file = 'Images/game.ico',
                                             mime = 'image/vnd.microsoft.icon'),
                    alt='WOWS Icon SVG'
           )),
      footer = tagList(modalButton("Return"))
    ))})  
}
  