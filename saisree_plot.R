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

###################    DATA MANIPULATION      ################################################################################################

path <- '2015_2021-quarterly-surgical_wait_times.xlsx'
qdata <- read_excel(path)

# Cleaned column names
colnames(qdata) <- tolower(colnames(qdata))
qdata <- qdata %>%
  rename(year = fiscal_year,
         hospital = hospital_name,
         procedure = procedure_group,
         wait_time_50 = completed_50th_percentile,
         wait_time_90 = completed_90th_percentile)

#convert <5 string to median value of 3
qdata <- mutate_if(qdata, is.character, str_replace_all, pattern = "<5", replacement = "3")

# correct datatypes of columns, simplify fiscal year to year at start of first quarter
qdata$waiting <- as.numeric(qdata$waiting)
qdata$completed <- as.numeric(qdata$completed)
qdata$year <- qdata$year %>% str_replace('(/).*', "")


# correct datatypes of columns, simplify fiscal year to year at start of first quarter
qdata$waiting <- as.numeric(qdata$waiting)
qdata$completed <- as.numeric(qdata$completed)
qdata$year <- qdata$year %>% str_replace('(/).*', "")

clean <- qdata %>% 
  drop_na() 

count <- select(qdata, -wait_time_50, -wait_time_90)

main <- clean %>%
  filter(procedure != 'All Procedures', 
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')
count <- count %>%
  filter(procedure != 'All Procedures', 
         hospital != 'All Facilities',
         health_authority != 'All Health Authorities')
all <-  clean %>% 
  filter(procedure == 'All Procedures', 
         hospital == 'All Facilities',
         health_authority == 'All Health Authorities') 


recent <- main %>% 
  filter(year >= 2017)
count_recent <- count %>%
  filter(year >= 2017)

authority <- count_recent %>% 
  group_by(health_authority, year, quarter) %>% 
  summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>% 
  arrange(desc(total_waiting))

no_cataract <- recent %>% 
  filter(procedure != 'Cataract Surgery')  

filtering_procedures <- function(health_authority,year){
  
}
app <- dash_app()

# Set the layout of the app

# year slider
yr_slider <- htmlDiv(
  list(
    dccRangeSlider(
      id="year_slider",
      min = 2009,
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

app %>% set_layout(list(
  yr_slider,
  ha_buttons
)
)



app$run_server(debug = T)


