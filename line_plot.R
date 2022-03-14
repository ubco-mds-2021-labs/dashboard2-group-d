library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(devtools)
library(dashBootstrapComponents)
library(plotly)


library(tidyverse)
library(readxl)
library(hexbin)
library(ggthemes)
library(repr)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)


# read the data
# path <- '2009_2021-quarterly-surgical_wait_times.xlsx'
qdata_2015_2022 <- read_csv("qdata_2015_2022.csv", col_select=c(2:10))
# head(qdata_2015_2022,2)

# groupby authority, year, quarter
authority <- qdata_2015_2022 %>%
  group_by(health_authority, year, quarter) %>%
  summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>%
  arrange(desc(total_waiting))


# add ratio of completed to total
authority_comp_prop <- authority %>%
  mutate(ratio = total_completed/(total_completed+total_waiting))


# ##############################################
# line chart function with the data grouped by health authority for a date range
comp_prop_plot <- function(years, authority){
  data_compprop <- authority_comp_prop %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  compprop_plot <- ggplot(data_compprop)+
    aes(x = year,
        y = ratio,
        color = quarter)+
    geom_point(size = 3)+
    geom_line(
      aes(group = quarter), size = 1)+
    labs(x = "Year", y = "Ratio", color = "")+ 
    ggtitle("Proportion of Completed Cases")+
    theme(
      legend.position = "bottom",
      text = element_text(size=16)
    )
  return(ggplotly(compprop_plot))
}


# Cards functions
wait_cases_count <- function(years, authority){
  waitcases <- authority_comp_prop %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  totalwaitcases <- sum(waitcases$total_waiting)
  return(totalwaitcases)
}

completed_cases_count <- function(years, authority){
  completecases <- authority_comp_prop %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  totalcompletecases <- sum(completecases$total_completed)
  return(totalcompletecases)
}

wait_50_mean <- function(years, authority){
  wait50 <- qdata_2015_2022 %>% drop_na() %>%  
    filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  meanwait50 <- mean(wait50$wait_time_50)
  return(trunc(meanwait50))
}

wait_90_mean <- function(years, authority){
  wait90 <- qdata_2015_2022 %>% drop_na() %>%
    filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  meanwait90 <- mean(wait90$wait_time_50)
  return(trunc(meanwait90))
}

##############################################
app <- dash_app(suppress_callback_exceptions = TRUE)

# Set the layout of the app


#year <- to_vec(for (x in seq("2007","2022")) paste(x,"=",x) )

# 1st plot - proportion of completed cases
comp_prop_graphic <- comp_prop_plot(c(2017,2020), 'Fraser')
plot1 <- dccGraph(id = 'prop_plot1', figure=comp_prop_graphic)


# All the score cards

wait_cases_card <- dbcCard(
  list(
    dbcCardHeader("Total Waiting Cases"),
    dbcCardBody(
      list(
        p(wait_cases_count, className = "card-title", id="wait_cases_text")
      )
    )
  ),
  style = list(width = "18rem")
)

completed_cases_card <- dbcCard(
  list(
    dbcCardHeader("Total Completed Cases"),
    dbcCardBody(
      list(
        p(completed_cases_count, className = "card-title", id="completed_cases_text")
      )
    )
  ),
  style = list(width = "18rem")
)

wait_50_card <- dbcCard(
  list(
    dbcCardHeader("Mean waiting time(weeks) - 50 %le"),
    dbcCardBody(
      list(
        p(wait_50_mean, className = "card-title", id="mean_waiting_time_50%_text")
      )
    )
  ),
  style = list(width = "18rem")
)

wait_90_card <- dbcCard(
  list(
    dbcCardHeader("Mean waiting time(weeks) - 90 %le"),
    dbcCardBody(
      list(
        p(wait_90_mean, className = "card-title", id="mean_waiting_time_90%_text")
      )
    )
  ),
  style = list(width = "18rem")
)


# year slider
yr_slider <- htmlDiv(
  list(
    dccRangeSlider(
      id="year_slider",
      min = 2015,
      max = 2022,
      marks=list(
        "2015" = "2015",
        "2016" = "2016",
        "2017" = "2017",
        "2018" = "2018",
        "2019" = "2019",
        "2020" = "2020",
        "2021" = "2021",
        "2022" = "2022"
      ),
      step=1,
      value=list(2015,2022)
    )
  )
)

#health_authority buttons
ha_buttons <- htmlDiv(
  list(
    dccDropdown(
      id="health_authority_buttons",
      options = list(list(label = "Interior", value = "Interior"),
                     list(label = "Fraser", value = "Fraser"),
                     list(label = "Vancouver Coastal", value = "Vancouver Coastal"),
                     list(label = "Vancouver Island", value = "Vancouver Island"),
                     list(label = "Northern", value = "Northern"),
                     list(label = "Provincial", value = "Provincial Health Services Authority")),
      value = 'Interior'
    )
  )
)

######################################3


app %>% set_layout(list(
  yr_slider,
  ha_buttons,
  plot1,
  cards <- dbcRow(
    list(
      dbcCol(wait_cases_card, width = 4),
      dbcCol(completed_cases_card, width = 8),
      dbcCol(wait_50_card, width = 4),
      dbcCol(wait_90_card, width = 8)      
    )
  )
 )
)
  
########Callbacks########

### 1st plot - callback
app$callback(
  output('prop_plot1', 'figure'),
  list(input('year_slider', 'value'),
       input('health_authority_buttons', 'value')),
  function(year, authority) {
    return(comp_prop_plot(year, authority))
  }
)


### score cards - callbacks
app$callback(
  output('wait_cases_text', 'children'),
  list(input('year_slider', 'value'),
       input('health_authority_buttons', 'value')),
  function(year, authority) {
    return(wait_cases_count(year, authority))
  }
)

app$callback(
  output('completed_cases_text', 'children'),
  list(input('year_slider', 'value'),
       input('health_authority_buttons', 'value')),
  function(year, authority) {
    return(completed_cases_count(year, authority))
  }
)

app$callback(
  output('mean_waiting_time_50%_text', 'children'),
  list(input('year_slider', 'value'),
       input('health_authority_buttons', 'value')),
  function(year, authority) {
    return(wait_50_mean(year, authority))
  }
)

app$callback(
  output('mean_waiting_time_90%_text', 'children'),
  list(input('year_slider', 'value'),
       input('health_authority_buttons', 'value')),
  function(year, authority) {
    return(wait_90_mean(year, authority))
  }
)


##########################
app$run_server(debug = T)


