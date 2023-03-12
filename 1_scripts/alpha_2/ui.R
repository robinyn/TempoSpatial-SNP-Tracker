library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(shinyWidgets)
library(RSQLite)

navbarPage("SNP Tracker", id="navbar", 
           tabPanel("Map", 
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id="controlPanel", class="panel panel-default", fixed=TRUE, draggable=TRUE,
                                      top=60,  left="auto", right=20, bottom="auto",
                                      width=330, height="auto",
                                      
                                      fluidPage(
                                        h1("Controls"),
                                        radioButtons("num_SNP", "Number of SNPs to track", choices = 1:2, inline=TRUE, selected=1),
                                        fluidRow(
                                          column(width=4, selectInput("CHR_choice1", "Chromosome", CHR_selection)),
                                          column(width=7, selectizeInput("SNP_choice1", "SNP1", choices=NULL, selected=NULL))
                                        ),
                                        conditionalPanel(
                                          condition="input.num_SNP==2",
                                          fluidRow(
                                            column(width=4, selectInput("CHR_choice2", "Chromosome", CHR_selection)),
                                            column(width=7, selectizeInput("SNP_choice2", "SNP2", choices=NULL, selected=NULL))
                                          )
                                        ),
                                        
                                        radioButtons("grouping_var", "Grouping variable", choices = c("None", "Country", "Distance"), 
                                                     inline=TRUE, selected="None"),
                                        
                                        conditionalPanel(
                                          condition="input.grouping_var=='Distance'",
                                          sliderInput("group_dist_input", "Distance in km", min=0, max=5000, step=1, value=100, ticks=TRUE),
                                        ),
                                        
                                        sliderInput("timestep", label="Time step", min=100, max=5000, value=time_step, step=100, ticks=TRUE),
                                        
                                        sliderTextInput("timeline", label="Timeline", 
                                                        choices = seq(from=time_range[1], to=time_range[2]+time_step, by=-time_step),
                                                        selected = time_range[1],
                                                        width="100%",
                                                        post=" BP",
                                                        animate=animationOptions(interval=2000, loop=TRUE))
                                      ),
                                      
                                      textOutput("textbox")
                                      )
                        )
                    )
           )