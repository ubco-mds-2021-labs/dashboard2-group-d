library(dash)
library(dashHtmlComponents)

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
qdata <- read_excel("/Users/alicesong/A-gitmds/551/551project/dashboard2-group-d/data/2009_2021-quarterly-surgical_wait_times.xlsx")
head(qdata,2)














app <- dash_app()

# Set the layout of the app


#year <- to_vec(for (x in seq("2007","2022")) paste(x,"=",x) )

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

app %>% set_layout(list(
  yr_slider,
  ha_buttons
  )
)
  


app$run_server(debug = T)


