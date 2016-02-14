
library(shiny)
library(rCharts)

shinyUI(
  navbarPage("Storm Database Analysis",
             tabPanel("Plot",
                      sidebarPanel(
                        sliderInput("range", 
                                    "Range:", 
                                    min = 1950, 
                                    max = 2011, 
                                    value = c(1993, 2011),
                                    format="####"),
                        uiOutput("evtypeControls")
                       ),
                      
                      mainPanel(
                        tabsetPanel(
                          
                          # Data by state
                          tabPanel(p(icon("map-marker"), "By Impact Category"),
                                   column(7,
                                          wellPanel(
                                            radioButtons(
                                              "impactCategory",
                                              "Impact category:",
                                              c("Population impact" = "population", "Economic impact" = "economic"))
                                          )
                                   ),
                                   
                                   column(7,
                                           plotOutput("impactByState")
                                           #plotOutput("economicImpactByState")
                                   )
                                   
                          ),
                          
                          # Data by event type
                          tabPanel(p(icon("line-chart"), "By event type"),
                                   h4('Population impact by event type', align = "center"),
                                   showOutput("populationImpact", "nvd3"),
                                   h4('Economic impact by event type', align = "center"),
                                   showOutput("economicImpact", "nvd3")
                          )
                        )
                      )
                      
             ),
             
             tabPanel("About",
                      mainPanel(
                        includeMarkdown("include.md")
                      )
             )
  )
)