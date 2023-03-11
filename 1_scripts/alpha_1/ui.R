library(shiny)
library(shinythemes)
library(tidyverse)
library(readxl)
library(leaflet)
library(shinyWidgets)
source("./server.R")

ui = navbarPage(
  "SNP tracker",
  tabPanel(title="Map viewer", value="pan1", 
           leafletOutput("map"),
           sliderTextInput(
             "animation", "Year:",
             choices = seq(from=time_range[1], to=time_range[2]+timestep, by=-timestep),
             selected = time_range[1],
             width="100%",
             post=" BP",
             animate=animationOptions(interval=-timestep, loop=TRUE)
           )),
  tabPanel(title="Data explorer", value="pan2"),
)