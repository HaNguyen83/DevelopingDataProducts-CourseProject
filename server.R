library(shiny)

# Plotting 
library(ggplot2)
library(rCharts)
library(ggvis)

# Data processing libraries
library(data.table)
library(reshape2)
library(dplyr)

# Required by includeMarkdown
library(markdown)
library(mapproj)
library(maps)

# Load helper functions
source("helper.R", local = TRUE)


# Load data
states_map <- map_data("state")
dt <- fread('data/events.agg.csv') %>% mutate(EVTYPE = tolower(EVTYPE))
evtypes <- sort(unique(dt$EVTYPE))


# Shiny server 
shinyServer(function(input, output, session) {
  
  # Define and initialize reactive values
  values <- reactiveValues()
  values$evtypes <- evtypes
  
  # Create event type checkbox
  output$evtypeControls <- renderUI({
    checkboxGroupInput('evtypes', 'Event types', evtypes, selected=values$evtypes)
  })
  
 
  # Prepare data for maps
  dt.agg <- reactive({
    aggregate_by_state(dt, input$range[1], input$range[2], input$evtypes)
  })
  
  # Prepare data for event type
  dt.agg.event <- reactive({
    aggregate_by_event(dt, input$evtypes)
  })
  
 
  # Impact by state
  output$impactByState <- renderPlot({
    print(plot_impact_by_state (
      dt = compute_affected(dt.agg(), input$impactCategory),
      states_map = states_map, 
      year_min = input$range[1],
      year_max = input$range[2],
      title = "Impact By State %d - %d",
      fill = "Affected"
    ))
  })
  

  # Population impact by event type
  output$populationImpact <- renderChart({
    plot_impact_by_event(
      dt = dt.agg.event() %>% select(Event, Injuries, Fatalities),
      dom = "populationImpact",
      yAxisLabel = "Affected",
      desc = TRUE
    )
  })
  
  # Economic impact by state
  output$economicImpact <- renderChart({
    plot_impact_by_event(
      dt = dt.agg.event() %>% select(Event, Crops, Property),
      dom = "economicImpact",
      yAxisLabel = "Total damage (Million USD)"
    )
  })
})
