# modalsDispCode.R
# Author: Mark Randall
# Date: 11 June 2025
# Code to render footer modals

dispRefModal <- function(input,value) {
observeEvent(input[[value]], {
  showModal(modalDialog(
    title = HTML("<b><u>References:</u></b>"),
    size = "l",
    HTML("<ul>
              <li>Beadell, S. J. L. R. N. (1941). View from HMS HERMIONE of HMS 
              LEGION moving alongside the damaged and listing HMS ARK ROYAL 
              in order to take off survivors. In HMS_Ark_Royal_sinking_2.jpg (Ed.),
              jpg (Vol. 761x571px). Imperial War Museums(collection no. 4700-01): 
              Imperial War Museums.</li>
              <li>Wargaming.net. (2012-2025, 2025). World of Warships. Wargaming.net. 
              Retrieved 10 May from 
              <a class = 'add_link' href='https://worldofwarships.asia/en/content/game/'></a></li>
              <li>Perry Swift. (2025). Potato Alert, A statistics companion app for 
              World Of Warships. razaqq. Retrieved 10 May from 
              <a class = 'add_link' href='https://github.com/razaqq/PotatoAlert'> </a></li>
              <li>Wikipedia. (2025, 25 April 2025). World of Warships. Wikipedia Foundation. 
              Retrieved 10 May from 
              <a class = 'add_link' href='https://en.wikipedia.org/wiki/World_of_Warships'></a> </li>
              <li>Mark Randall, & Perry Swift. (2015-2025). Potato Alert tertius_keen Spuds Record
              [Microsoft Access SQLIte].</li>
              <li>Daboin, C. J., & Laurent;, S. (2023). Why is the shinyBS tooltip not 
              showing up while using bslib? In (2025 ed.). stackoverflow: Stack OverFlow.<br>
              <a class = 'add_link' href='https://stackoverflow.com/questions/76755621/why-is-the-
              shinybs-tooltip-not-showing-up-while-using-bslib'></a></li>
              <li>Shiny. (2024, 2024). Themes. Shiny. Retrieved 9 Jun from 
              <a class = 'add_link' href='https://shiny.posit.co/r/articles/build/themes/'></a>


         </ul>"),
    footer = tagList(modalButton("Return"))
  ))})    
}

dispAccessModal <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "UML WOWS Access Database",
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
               HTML(paste0("&#8195;World of Warships &#169; is a 'free' multiplayer on line game developed by Wargaming. 
               The game was annouced as World of Battleships to complement two other games in the wargaming stable. 
               This annoucement occured 16 August 2011. The game conducted a closed beta testing 12 March 2015 
               with a name change of World of Warships(2012). Open beta testing commenced 2 July 2015. 
               The microsoft windows official launch was 17 September 2015 and in the Microsoft Store and Steam 
               15 November 2015.<br> ",
               "&#8195;The Game genre is considered massively multiplayer online(MMO)/Vehicular Combat Game/
               third person shooter(TPS). It has games clients for the following
               platforms Microsoft Windows, macOS (discontinued), iOS, Android, PlayStation 4, PlayStation 5, 
               Xbox One, Xbox Series X/S.<br> ",
               "'&#8195;Players control warships of choice and can battle other random players on the server, 
               play cooperative battles against bots, or participate in an advanced player versus environment 
               (PvE) battle mode. For the most skilled players, two seasonal competitive modes are also available.'<br>",
               "(Wikipedia.2025, 25 April 2025)",
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
             
               HTML(paste0("&#8195;The Potato Alert programme was developed by Perry Swift in c++.  It is as GUI
                          that collects data from the World of Warships through application
                          programming interfaces(API).<br>",
                          
                          "&#8195The programme can be downloaded from the git hub site;  
                          <a class = 'add_link' href='https://github.com/razaqq/PotatoAlert'>link  </a> or 
                          installed via some other programmes that manage World of Warships sanctioned 
                          modifications. The preferred installation is via the git hub location as it stores the 
                          application in a different file and conducts automatic update searches.<br>",
                          
                          "&#8195;The game also stores to computer in:<br>",
                               "<ul>
                               <li>&#8195;C:/Users/&#60;USER&#62;/AppData/Local/PotatoAlert/Matches 
                                             a match csv file and a match history database created by SQLite.</li>
                               <li>&#8195;C:/&#60;USER&#62;/Mark/AppData/Local/PotatoAlert/PotatoAlert.log a file to 
                          enable error locations.</li>
                          </ul>",
                          "(Perry Swift,2025)",
                          sep = "<br>"
                          
                          
                          
                          )),
               )),
      footer = tagList(modalButton("Return")),
         ))})  
}#EOFnAbout

dispScreenshotAccessPng <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "Input Screen Access",
      size = "l",
      div( style='overflow:scroll',
           tags$img(src= base64enc::dataURI(file="Images/Access Input Screen.png",
                                            mime="image/png"), 
                    alt="Input Screen for WOWS Access Data Base",
                    width = "100%",
                    height = "100%")),
      footer = tagList(modalButton("Return"))
    ))})   
}#EOFStructure

dispScreenshotPAPng <- function(input,value) {
  observeEvent(input[[value]], {
    showModal(modalDialog(
      title= "Screen Shot Potato Alert",
      size = "l",
      div( style='overflow:scroll',
           tags$img(src= base64enc::dataURI(file="Images/PA 6_6_2025.png",
                                            mime="image/png"), 
                    alt="Input Screen for WOWS Access Data Base",
                    width = "140%",
                    height = "110%")),
      footer = tagList(modalButton("Return"))
    ))})   
}#EOFStructure
 