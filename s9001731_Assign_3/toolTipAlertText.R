# toolTipAlertText.R
# Author: Mark Randall
# Date: 11 June 2025
# Text for Overview Alert and Tooltips 

overview <- HTML(paste0("&#8195;This programme is written to examine personal data 
gathered while playing World of Warhips(Wargaming.net,2012-2025, 2025). 
To retrieve the data from the game a programme called Potato Alert(Perry Swift,2025) 
that connects with the game API is used. This was then loaded to a self written 
access data base. ",
"<br>",
"&#8195;This programme will only look at the type of battle and ship's influence on the 
teams win rate and damage differential and the chance of winning a match. Data 
is taken from an unpublished personal database(Mark Randall, & Perry Swift. (2015-2025)."))

selAttribute <-HTML(paste0("&#8195;The selection lists were meant to be conditional panels.
Their activation was meant to be determined by the selction of the radio box. 
Unfortunately conditional panels have not been able to be instigated. A secondary approach 
by using vanilla css through the stylesheet in the www file was tried as well. This alas 
did not work either. It appears that shinyBS and the later bslib(the shiny app favoured bootstrap)
are at odds with each other, create conflict and mask class calls. ShinyBS uses the much older
bootstrap 3 and bslib bootstrap 5. Which is odd as shiny defaults to bootstrap 3. This 
application could be forced to use bootstrap 5 with the addition of<br>
&#8195;theme = bs_theme(version =5, .....)<br>
However, this causes catastrophic layout clashes. As this cause was not discovered till 24 
hours before submission, or disclosed as a problem prior to building the app; refactoring
would have caused late submission.<br>
<b>The work around is that the active selection list is that corresponding to the radio button.
Using the non selected list does not affect the visuals. WORKS ALL PAGES</b><br>
(Daboin, C. J., & Laurent;, S. (2023))(Shiny. (2024, 2024) )"))