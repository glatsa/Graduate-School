

#
# This is a Shiny web application for interacting with Stock Data.
# Author: Graham Latsa
#
#########---######---

#Loading dependent packages  ----
require(shiny)
#rqrd_Pkg = c('shiny','plotly','plyr','tidyverse','ggmap')
#for(p in rqrd_Pkg){
  #if(!require(p,character.only = TRUE)) 
  #install.packages(p);
  #library(p,character.only = TRUE)
#}
#source("global.R")

source("ui.R")
source("server.R")

# Run the application 
shinyApp(ui = ui, server = server)
