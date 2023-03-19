library(shiny)
library(shinythemes)
library(tidyverse)
library(leaflet)
library(leaflet.minicharts)
library(shinyWidgets)
library(RSQLite)
library(plotly)

navbarPage("SNP Tracker", id="navbar", 
           tabPanel("Map", 
                    div(class="outer",
                        tags$head(includeCSS("styles.css")),
                        leafletOutput("map", width="100%", height="100%"),
                        
                        absolutePanel(id="mainPanel", class="panel panel-default", fixed=TRUE, draggable=TRUE,
                                      top=70,  left=10, right="auto", bottom="auto",
                                      width=450, height="auto",
                                      tabsetPanel(id="mainPanel_tabs", type="pills",
                                        #tags$br(),
                                        tabPanel("Controls",
                                          fluidPage(
                                            fluidRow(
                                              column(width=4, 
                                                     radioButtons("num_SNP", "No. of SNPs", 
                                                                  choices = 1:2, inline=TRUE, selected=1),
                                                     style='padding-left:10px; padding-right:0px; padding-top:15px; padding-bottom:0px;left-margin:-15px;'),
                                              column(width=4, 
                                                     radioButtons("plot_type", "Plot type", 
                                                                  choices = c("Pie", "Bar"), inline=TRUE, selected="Pie"),
                                                     style='padding-left:0px; padding-right:0px; padding-top:15px; padding-bottom:0px;left-margin:-15px;'),
                                              column(width=4, 
                                                     radioButtons("data_type", "Data type", 
                                                                  choices = c("Count", "Freq"), inline=TRUE, selected="Count"),
                                                     style='padding-left:0px; padding-right:0px; padding-top:15px; padding-bottom:0px')
                                            ),
                                            fluidRow(
                                              column(width=4, selectInput("CHR_choice1", "Chromosome", CHR_selection),
                                                     style='padding-left:10px; padding-right:1px; padding-top:0px; padding-bottom:0px'),
                                              column(width=5, selectizeInput("SNP_choice1", "SNP1", choices=NULL, selected=NULL),
                                                     style='padding-left:5px; padding-right:1px; padding-top:0px; padding-bottom:0px'),
                                              column(width=2, 
                                                     htmlOutput("SNP1_alleles"),
                                                     style='padding-left:6px; padding-right:17px; padding-top:23px; padding-bottom:10px')
                                            ), # /fluid row
                                            
                                            conditionalPanel(
                                              condition="input.num_SNP==2",
                                              fluidRow(
                                                column(width=4, selectInput("CHR_choice2", "Chromosome", CHR_selection),
                                                       style='padding-left:12px; padding-right:1px; padding-top:1px; padding-bottom:0px'),
                                                column(width=5, selectizeInput("SNP_choice2", "SNP2", choices=NULL, selected=NULL),
                                                       style='padding-left:2px; padding-right:1px; padding-top:1px; padding-bottom:0px'),
                                                column(width=2, 
                                                       htmlOutput("SNP2_alleles"),
                                                       style='padding-left:6px; padding-right:17px; padding-top:23px; padding-bottom:10px')
                                              ) # /fluid row
                                            ), # /conditional panel
                                            
                                            #tags$hr(),
                                            
                                            fluidRow(
                                              column(width=4,
                                                     prettySwitch(inputId = "showlabel", label="Show labels", fill=TRUE)
                                              ),
                                              column(width=4,
                                                     prettySwitch(inputId = "showmissing", label="Show missing data", fill=TRUE, value=TRUE)
                                              ),
                                            ),
                                            fluidRow(
                                              column(width=7,
                                                radioButtons("grouping_var", "Grouping variable", choices = c("None", "Country", "Distance"), 
                                                             inline=TRUE, selected="None"), 
                                                             style='padding-left:5px; padding-right:0px; padding-top:0px; padding-bottom:0px;'
                                              )
                                            ), # /fluid row
                                            conditionalPanel(
                                              condition="input.grouping_var=='Distance'",
                                              fluidRow(
                                                column(width=12,
                                                  sliderInput("group_dist_input", "Distance", 
                                                              width="100%", min=0, max=5000, step=1, value=100, ticks=TRUE, post=" km"),
                                                  style="margin-top:-10px;"
                                                )
                                              ) # /fluid row
                                            ), # /conditional panel
                                            fluidRow(
                                              column(width=6,
                                                sliderInput("timestep", label="Time step", 
                                                            width="100%", min=100, max=5000, value=time_step, step=100, ticks=TRUE, post=" years"),
                                                style="margin-top:-10px;"
                                              ),
                                              column(width=6,
                                                     sliderInput("windowsize", label="Window size", 
                                                                 width="100%", min=100, max=5000, value=time_step, step=100, ticks=TRUE, post=" years"),
                                                     style="margin-top:-10px;"
                                              )
                                            ), # /fluid row
                                            fluidRow(
                                              column(width=12,
                                                sliderTextInput("timeline", label="Timeline", 
                                                                choices = seq(from=time_range[1], to=time_range[2], by=-time_step),
                                                                selected = time_range[1],
                                                                width="100%",
                                                                post=" BP",
                                                                animate=animationOptions(interval=2000, loop=TRUE)),
                                                style="margin-top:-10px;"
                                              )
                                            ), # /fluid row
                                            fluidRow(
                                              column(width=4, selectInput("palette_type", "Palettes", choices=c(names(color_palettes)), 
                                                                          selected = "Qualitative"),
                                                     style='padding-left:10px; padding-right:1px; padding-top:1px; padding-bottom:1px'),
                                              column(width=4, selectInput("palette_choice", "Colors", choices=color_palettes[["Qualitative"]], 
                                                                          selected="Dark2"),
                                                     style='padding-left:5px; padding-right:1px; padding-top:1px; padding-bottom:1px'),
                                              column(width=4, selectInput("legend", "Legend", choices=c("None", "TR", "TL", "BR", "BL"), 
                                                                          selected="TR"),
                                                     style='padding-left:5px; padding-right:10px; padding-top:1px; padding-bottom:1px')
                                            ), # /fluid row
                                            
                                            fluidRow(
                                              column(width=6, sliderInput("opacity", label="Opacity", width="100%",
                                                     min=0, max=100, value=100, step=1, post="%", ticks=TRUE)),
                                              column(width=6, sliderInput("plot_size", label="Size", width="100%",
                                                                          min=1, max=100, value=25, step=1, post="%", ticks=TRUE))
                                            ) # /fluid row
                                          ) # /fluid page
                                        ), # /controls tab panel
                                        tabPanel("Summary",
                                          fluidPage(
                                            h2("Overall summary"),
                                            fluidRow(
                                              column(width=12, plotlyOutput("total_summary_SNP1", height=200),
                                                     style='padding-left:5px; padding-right:5px; padding-top:5px; padding-bottom:1px')
                                            ), # /fluid row
                                            conditionalPanel(
                                              condition="input.num_SNP==2",
                                              fluidRow(
                                                column(width=12, plotlyOutput("total_summary_SNP2", height=200),
                                                       style='padding-left:5px; padding-right:5px; padding-top:10px; padding-bottom:1px')
                                              ) # /fluid row
                                            ), # /conditional panel
                                            # fluidRow(
                                            #   selectInput("summary_choices", "Clusters",
                                            #               choices = c("UK", "US", "Sweden"), multiple=TRUE)
                                            # ) #/fluid row
                                          ) # /fluid page
                                        ) # /summary tab panel
                                      ) # /tabset panel
                                      ) # /absolute panel
                        ) # /div outer
                    ), # /Map tab
           tabPanel("Data explorer",
                    fluidPage(
                      fluidRow(
                        column(width=2, selectInput("dataset", "Dataset", choices=sql_datasets), offset = 0,
                               style='padding-left:50px; padding-right:5px; padding-top:10px; padding-bottom:5px'),
                        column(width=2, selectInput("exploreData", "Data type", choices=c("Samples", "SNPs")), offset = 0,
                               style='padding-left:0px; padding-right:5px; padding-top:10px; padding-bottom:5px'),
                        column(width=8, uiOutput("exploreUI"), offset = 0,
                               style='padding-left:0px; padding-right:5px; padding-top:10px; padding-bottom:5px')
                      ),
                      # fluidRow(
                      #   column(width=12, checkboxGroupInput("exploreColumns", "Columns", choices=NULL))
                      # ),
                      fluidRow(
                        column(width=12, dataTableOutput("table1"))
                      )
                    )
                    ) # /Data explorer tab
           ) # /navbar page