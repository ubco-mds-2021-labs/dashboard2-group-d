library(tidyverse)
library(readxl)
library(hexbin)
library(ggthemes)
library(repr)
library(lubridate)
library(stringr)
library(reshape2)
library(gridExtra)

#load data
qdata <- read.xlsx('data/final_data_2015_2022.xlsx')

# drop rows with NAs
clean <- qdata %>% drop_na()
count <- select(qdata, -wait_time_50, -wait_time_90)

#cataract surgery removed
not_cataract <- clean %>% filter(procedure != "Cataract Surgery")

# group by health authority
authority_data <- not_cataract %>% 
    group_by(health_authority, year, quarter) %>% 
    summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>% 
    arrange(desc(total_waiting))

# add ratio of completed to waitingAndcompleted
authority_comp_prop <- authority %>%
    mutate(ratio = total_completed/(total_completed+total_waiting))

#prep dataframe for plotting
melted_data <- melt(authority_data) %>% unite(time, year, quarter, sep = "")
print(melted_data)

app <- dash_app()

#year slider
yr_slider <- htmlDiv(
  list(
    dccRangeSlider(
      id="year_slider",
      min = 2015,
      max = 2022,
      marks=list("2015" = "2015", "2016" = "2016", "2017" = "2017", "2018" = "2018",
                 "2019" = "2019", "2020" = "2020", "2021" = "2021", "2022" = "2022"
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
# hosp_dropdown <- html.Div(
#   list(
#     dccDropdown(
#       id='hospital_dropdown',
#       options=(),
#       value=(),
#       clearable=False
#     )
#   )
# )

#wait_complete plot

# wc_plot <- melted_data %>% 
#   ggplot(hospname, aes(x = time, y = value, fill = variable)) + 
#   geom_bar(stat = "identity", position = 'dodge') +
#   labs(y = "Case Count",
#       x = 'Time',
#         title = "Total waiting and completed cases", fill = ""
#   ) +
#   theme(text = element_text(size=16),
#               axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
#               legend.position="top"
#   )

#app configuration
app %>% set_layout(list(
  yr_slider,
  ha_buttons,
#  hosp_dropdown
  )
)
  


app$run_server(debug = T)