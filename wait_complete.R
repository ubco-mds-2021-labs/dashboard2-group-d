library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(tidyverse)
library(readxl)
library(hexbin)
library(ggthemes)
library(repr)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

#data manipulation

qdata <- read_excel('final_data_2015_2022.xlsx')
qdata <- qdata[,-1]

#convert <5 string to median value of 3
qdata <- mutate_if(qdata, is.character, str_replace_all, pattern = "<5", replacement = "3")

clean <- qdata %>%
  drop_na()

count <- select(clean, -wait_time_50, -wait_time_90)

main <- clean %>%
  filter(procedure != 'All Procedures',
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')

count <- count %>%
  filter(procedure != 'All Procedures',
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')

all <- clean %>%
  filter(procedure == 'All Procedures',
         hospital == 'All Facilities',
         health_authority == 'All Health Authorities')

no_cataract <- count %>% 
  filter(procedure != 'Cataract Surgery')  

no_cataract$waiting <- as.numeric(as.character(no_cataract$waiting))
no_cataract$completed <- as.numeric(as.character(no_cataract$completed))

hosp_list <- unique(no_cataract$hospital)
new_list <- list()
for(i in hosp_list){
  sublist <- list(label = i, value = i)
  new_list <- append(new_list, list(sublist))
}

no_cataract <- no_cataract %>% filter(health_authority==health_authority,year>=year[1],year<=year[2])

hosp_data <- function(health_authority, year, hosp){
  no_cataract <- no_cataract %>% filter(health_authority==health_authority,year>=year[1],year<=year[2])

  hospital_data <- no_cataract %>% 
      group_by(hospital, year, quarter) %>% 
      summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>% 
      unite(time, year, quarter, sep = "") %>%
      arrange(desc(total_waiting))
  melted_hosp_data <- melt(hospital_data) %>% filter(hospital == hosp)

  melted_hosp_data

}

# wait_complete_plot <- function(health_authority, year, hosp){
#   one_hosp_data <- hosp_data(health_authority, year, hosp)
  
#   wc_plot <- 
#       ggplot(one_hosp_data, aes(x = time, y = value, fill = variable))+ 
#           geom_bar(stat = "identity", position = 'dodge') +
#           labs(y = "", x = 'Time', title = "Total waiting and completed cases", fill = "") +
#           theme(text = element_text(size=16), 
#                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#                 legend.position="top")

#   wc_plot
#}

app <- dash_app()

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
      value=list(2017,2022)
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

#hospital dropdown
hosp_drop <- htmlDiv(
  list(
    dccDropdown(
      id="hosp_drop",
      options = new_list,
      value = 'Kelowna General Hospital'
    )
  )
)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app %>% set_layout(list(
  yr_slider,
  ha_buttons,
  hosp_drop,
  dccGraph(id = "wait_complete")
)
)

app$callback(
  output("wait_complete", "figure"),
  list(input("health_authority_buttons", "value"),
       input("year_slider", "value"),
       input("hosp_drop", "value")),
  function(health_authority="Interior", year=c(2017,2022), hospital="Kelowna General Hospital") {
    one_hosp_data <- hosp_data(health_authority, year, hospital)
    wc_plot <- 
      ggplot(one_hosp_data, aes(x = time, y = value, fill = variable))+ 
          geom_bar(stat = "identity", position = 'dodge') +
          labs(y = "", x = 'Time', title = "Total waiting and completed cases", fill = "") +
          theme(text = element_text(size=16), 
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                legend.position="top")
    ggplotly(wc_plot)
  }
)

app$run_server(debug = T)