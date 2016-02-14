#' Aggregate dataset by state
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_state <- function(dt, year_min, year_max, evtypes) {
  replace_na <- function(x) ifelse(is.na(x), 0, x)
  round_2 <- function(x) round(x, 2)
  
  states <- data.table(STATE=sort(unique(dt$STATE)))
  
  aggregated <- dt %>% filter(YEAR >= year_min, YEAR <= year_max, EVTYPE %in% evtypes) %>%
    group_by(STATE) %>%
    summarise_each(funs(sum), COUNT:CROPDMG)
  
  # We want all states to be present even if nothing happened
  left_join(states,  aggregated, by = "STATE") %>%
    mutate_each(funs(replace_na), FATALITIES:CROPDMG) %>%
    mutate_each(funs(round_2), PROPDMG, CROPDMG)    
}

#' Aggregate dataset by year
#' 
#' @param dt data.table
#' @param year_min integer
#' @param year_max integer
#' @param evtypes character vector
#' @return data.table
#'
aggregate_by_event <- function(dt, evtypes) {
  round_2 <- function(x) round(x, 2)
  
  # Filter
  dt %>% filter(EVTYPE %in% evtypes) %>%
    # Group and aggregate
    group_by(EVTYPE) %>% summarise_each(funs(sum), COUNT:CROPDMG) %>%
    # Round
    mutate_each(funs(round_2), PROPDMG, CROPDMG) %>%
    rename(
      Event = EVTYPE, Count = COUNT,
      Fatalities = FATALITIES, Injuries = INJURIES,
      Property = PROPDMG, Crops = CROPDMG
    )
}


#' Add Affected column based on category
#'
#' @param dt data.table
#' @param category character
#' @return data.table
#'
compute_affected <- function(dt, category) {
  dt %>% mutate(Affected = {
    if(category == 'population') {
      INJURIES + FATALITIES
    } else if(category == 'economic') {
      PROPDMG + CROPDMG
    }
  })
}


#' Prepare map of economic or population impact
#' 
#' @param dt data.table
#' @param states_map data.frame returned from map_data("state")
#' @param year_min integer
#' @param year_max integer
#' @param fill character name of the variable
#' @param title character
#' @param low character hex
#' @param high character hex
#' @return ggplot
#' 
plot_impact_by_state <- function (dt, states_map, year_min, year_max, fill, title, low = "#fff5eb", high = "#d94801") {
  title <- sprintf(title, year_min, year_max)
  p <- ggplot(dt, aes(map_id = STATE))
  p <- p + geom_map(aes_string(fill = fill), map = states_map, colour='black')
  p <- p + expand_limits(x = states_map$long, y = states_map$lat)
  p <- p + coord_map() + theme_bw()
  p <- p + labs(x = "Long", y = "Lat", title = title)
  p + scale_fill_gradient(low = low, high = high)
}



#' Prepare plots of impact by year
#'
#' @param dt data.table
#' @param dom
#' @param yAxisLabel
#' @param desc
#' @return plot
#' 
plot_impact_by_event <- function(dt, dom, yAxisLabel, desc = FALSE) {
  impactPlot <- nPlot(
    value ~ Event, group = "variable",
    data = melt(dt, id="Event") %>% arrange(Event, if (desc) { desc(variable) } else { variable }),
    type = "multiBarHorizontalChart", dom = dom, width = 650
  )
  impactPlot$chart(margin = list(left = 100))
  impactPlot$yAxis(axisLabel = yAxisLabel, width = 80)
  impactPlot$xAxis(axisLabel = "Event", width = 70)
  
  impactPlot
}


