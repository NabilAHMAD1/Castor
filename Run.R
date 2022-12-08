library(shiny)
# install all packages needed
install.packages(c("sp","sf","tmap","leaflet"))
remotes::install_github("bnosac/BelgiumMaps.StatBel")
# Load all packages
library(BelgiumMaps.StatBel)
library(sp)
library(sf)
library(tmap)
library(leaflet)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(4242)
)
