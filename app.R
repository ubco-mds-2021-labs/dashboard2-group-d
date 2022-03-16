library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(dashBootstrapComponents)

library(devtools)
library(ggplot2)
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


library(EBImage)
library(usmap)

####### Set current directory as working directory
# setwd("~/A-gitmds/551/551project/dashboard2-group-d")

####### read the data
qdata <- read_csv("data/qdata_2015_2022.csv", col_select=c(2:10))
qdata$year <- as.integer(qdata$year)

####### data manipulation
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

all <- clean %>%
  filter(procedure == 'All Procedures',
         hospital == 'All Facilities',
         health_authority == 'All Health Authorities')

no_cataract <- main %>% 
  filter(procedure != 'Cataract Surgery') 

############## data manipulation - 1st plot

# data with complete ratio in specific authority and specific year

comppropdata <- function(years, authority){
  qdata <- qdata %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  
  authority <- qdata %>%
    group_by(health_authority, year, quarter) %>%
    summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>%
    arrange(desc(total_waiting))
  # add column ratio of completed to total
  authority_comp_prop <- authority %>%
    mutate(ratio = total_completed/(total_completed+total_waiting))
  # data with parameters
  authority_comp_prop
}



############## data manipulation - 3rd plot

filtering_procedures <- function(ha_authority,yr,pace){
  no_cataract<-no_cataract %>% filter(health_authority==ha_authority,year>=yr[1], year<=yr[2]) 
  
  # by procedure group 
  procedure <- no_cataract %>% 
    group_by(procedure, year, quarter) %>% 
    summarise(wait_time_50 = mean(wait_time_50), wait_time_90 = mean(wait_time_90)) %>% 
    arrange(desc(wait_time_90))
  
  procedure_unite <- procedure %>% 
    unite(time, year, quarter, sep = "")
  
  procedure_order <- procedure_unite %>% group_by(procedure) %>% summarize(mean_wait_time_90_procedure=round(mean(wait_time_90),2)) %>% arrange(desc(mean_wait_time_90_procedure)) 
  fastest <- head(procedure_order,5)
  slowest <- tail(procedure_order,5)
  result  <- fastest
  if(pace=="Slowest"){
    result <- slowest
  }
  result
}

############## data manipulation - 4th plot

# hospital list for dropdown
hosp_list <- unique(no_cataract$hospital)
new_list <- list()
for(i in hosp_list){
  sublist <- list(label = i, value = i)
  new_list <- append(new_list, list(sublist))
}

# data for 4th plot
hosp_data <- function(years, hosp){
  hosp_data_a_y_h <- no_cataract %>% filter(year>=years[1],year<=years[2])
  
  hospital_data <- hosp_data_a_y_h %>% 
    group_by(hospital, year, quarter) %>% 
    summarise(total_waiting = sum(waiting), total_completed = sum(completed)) %>% 
    unite(time, year, quarter, sep = "") %>%
    arrange(desc(total_waiting))
  melted_hosp_data <- melt(hospital_data) %>% filter(hospital == hosp)
  
  melted_hosp_data
}


################################################
####### plot functions

# 1st plot function
comp_prop_plot <- function(years, authority){
  data_compprop <- comppropdata(years, authority)
  
  compprop_plot <- ggplot(data_compprop)+
    aes(x = year,
        y = ratio,
        color = quarter)+
    geom_point(size = 3)+
    geom_line(
      aes(group = quarter), size = 1)+
    labs(x = "Year", y = "Ratio", color = "")+ 
#    ggtitle("Proportion of Completed Cases")+
    theme(
      legend.position = "bottom",
      text = element_text(size=16)
    )
  return(ggplotly(compprop_plot))
}


# 2nd map function
map <- function(authority = "Interior"){
  
  if(authority == "Interior"){
    img = readImage('data/images/interior.png')
  }
  if(authority == "Fraser"){
    img = readImage('data/images/fraser.png')
  }
  if(authority == "Vancouver Coastal"){
    img = readImage('data/images/vancoastal.png')
  }
  if(authority == "Vancouver Island"){
    img = readImage('data/images/vanisland.png')
  }
  if(authority == "Northern"){
    img = readImage('data/images/northern.png')
  }
  if(authority == "Provincial Health Services Authority"){
    img = readImage('data/images/provincial.png')
  }
  
  map_plot <- plot_ly() %>%
    layout(
      images = list(
        source = raster2uri(as.raster(img)),
        x = 0, y = 0,
        sizex = 4, sizey = 4,
        xref = "x", yref = "y",
        xanchor = "left", yanchor = "bottom"
        #sizing = "stretch"
      ),
      xaxis=list(showticklabels=FALSE, zerolinecolor = '#ffff',showgrid = F),
      yaxis=list(showticklabels=FALSE, zerolinecolor = '#ffff', showgrid = F),
      xaxis = list(showgrid = FALSE),
      yaxis = list(showgrid = FALSE)
    )
  return(map_plot)
}



# 3rd plot function
procedure_plots <- function(health_authority,year,pace){
  result <- filtering_procedures(health_authority,year,pace)
  
  pace_plot <- 
    ggplot(result, aes(y = procedure, x = mean_wait_time_90_procedure,fill=procedure))+ 
    geom_bar(stat="identity",show.legend=FALSE,alpha=0.6)+
    scale_fill_manual(values=c("steelblue3","gold3","black","navyblue","grey"))+
    geom_text(aes(label = mean_wait_time_90_procedure), hjust = 2,color="black")+
    labs(x = "Wait Time (weeks)"
#         ,title="Fastest/Slowest Treated Procedures"
         )+               
    theme_classic()+
    theme(axis.text=element_text(size=10),
          title=element_text(size=12),
          axis.title=element_text(size=10)       
    )
  return(ggplotly(pace_plot))
}

# 4th plot function
wait_complete_plot <- function(year, hosp){
  one_hosp_data <- hosp_data(year, hosp)
  wc_plot <- 
    ggplot(one_hosp_data, aes(x = time, y = value, fill = variable))+ 
    geom_bar(stat = "identity", position = 'dodge') +
    labs(y = "", x = 'Time in YearQuarter') +
    theme(legend.position="top",
    axis.text.x=element_blank())
  return(ggplotly(wc_plot))
}

################################################
####### card functions

wait_cases_count <- function(years, authority){
  authority_comp_prop <- comppropdata(years, authority)
  waitcases <- authority_comp_prop %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  totalwaitcases <- sum(waitcases$total_waiting)
  return(totalwaitcases)
}

completed_cases_count <- function(years, authority){
  authority_comp_prop <- comppropdata(years, authority)
  completecases <- authority_comp_prop %>% filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  totalcompletecases <- sum(completecases$total_completed)
  return(totalcompletecases)
}

wait_50_mean <- function(years, authority){
  wait50 <- clean %>%  
    filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  meanwait50 <- mean(wait50$wait_time_50)
  return(round(meanwait50))
}

wait_90_mean <- function(years, authority){
  wait90 <- clean %>%
    filter( (year>=years[1] & year<=years[2]) & (health_authority==authority))
  meanwait90 <- mean(wait90$wait_time_50)
  return(round(meanwait90))
}

##############################################


# Set the layout of the app

### All the plots
## 1st plot - proportion of completed cases
comp_prop_graphic <- comp_prop_plot(c(2017,2020), 'Fraser')
plot1 <- dccGraph(id = 'prop_plot1', figure=comp_prop_graphic)

## 2nd map - authorities
map_graphic <- map("Interior")
plot2 <- dccGraph(id = 'map-bc', figure=map_graphic)


## 3rd plot - Fastest and Slowest procedures
#pace buttons
pace_button <- htmlDiv(
  list(
    dccDropdown(
      id="pace_dropdown",
      options = list(list(label = "Fastest", value = "Fastest"),
                     list(label = "Slowest", value = "Slowest")),
      value = 'Fastest'
    )
  )
)
#3rd plot 
procedure_graphic <- procedure_plots("Interior", c(2018,2021), "Fastest")
plot3 <- dccGraph(id = 'procedure_plot3', figure=procedure_graphic)

## 4th plot - waiting and completed 
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
#4th plot
wait_complete_graphic <- wait_complete_plot(c(2017,2020), "Kelowna General Hospital")
plot4 <- dccGraph(id = 'wait_complete_plot4', figure=wait_complete_graphic)


### All the score cards

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

##########Layout Container###########################
# row 1 - has header
row1 <- dbcRow(
  h1("Surgical Wait Time - BC", 
     style=list('background-color' = '#000080', # navy blue 
                'color' = 'white', 
                'font-weight' = 'bolder', 
                'padding-top' = '15px',
                'padding-bottom' = '15px',
                'bottom-margin' = '0',
                "textAlign" = "center",  
                "font-size" = 40, 
                "margin-top" = 10)
  )
)

#### row 2 ############## has authority dropdown and slider
row2 <- div(
  dbcRow(
    list(
      dbcCol(div(), width = 1),
      dbcCol(ha_buttons, width = 3),
      dbcCol(div(), width = 1),
      dbcCol(yr_slider, width = 6)
    ),
    style=list('color' = '#000080',  # navy blue
               'background-color' = '#D3D3D3',  # light grey
               'font-weight' = 'bolder',
               'font-size' = '15px',
               'padding-top' = '20px',
               'padding-bottom' = '20px',
               'top-margin' = '0',
               'left-mergin' = '5',
               'text-align' = "center",
               'height' = '10')
  )
)

############ row 3 (has score cards in first column and all plots in 2nd column) ###############################

# 1st column of row 3 (has 4 score cards)
col1 <- div(list(         
  dbcCol(list(
    dbcRow(wait_cases_card),
    br(),
    dbcRow(completed_cases_card),
    br(),
    dbcRow(wait_50_card),
    br(),
    dbcRow(wait_90_card)
  )
  )
),
style=list(
  'font-family' = 'times new roman',
  'padding-left' = 0, 
  "padding-right" = 0,
  'text-align' = "center")
)

###### 2nd column of row 3 (this column has five rows) ################

# This row has titles of top two plots
top_left_title <- dbcRow(div("Proportion of completed cases", style = list("font-weight" = "bolder", 'text-align' = "center")))
top_right_title <- dbcRow(div("Health authority", style = list("font-weight" = "bolder", 'text-align' = "center")))

col2_row1 <- div(list(
  dbcRow(list(
    dbcCol(top_left_title, width = 6),
    dbcCol(top_right_title, width = 6)
  )
  ))
)

# This row has top 2 top two plots
top_left_plot <- dbcRow(plot1)
top_right_plot <- dbcRow(plot2)

col2_row2 <- div(list(
  dbcRow(list(
    dbcCol(top_left_plot, width = 6),
    dbcCol(top_right_plot, width = 6)
  )
  )), 
  style=list("padding-bottom" = 0)
)

# This row has titles of bottom two plots
bottom_left_title <- dbcRow(div("Fastest/Slowest treated procedures", style = list("font-weight" = "bolder", 'text-align' = "center")))
bottom_right_title <- dbcRow(div("Total completed and waiting cases in Hospital", style = list("font-weight" = "bolder", 'text-align' = "center")))

col2_row3 <- div(list(
  dbcRow(list(
    dbcCol(bottom_left_title, md = 6),
    dbcCol(bottom_right_title, md = 6)
  )
  ))
)

# This row has pace-buttons and dropdown of hospital plot
pace_buttons_col <- dbcRow(pace_button)
hosp_dropdown_col <- dbcRow(hosp_drop)

col2_row4 <- div(list(
  dbcRow(list(
    dbcCol(pace_buttons_col, md = 6),
    dbcCol(hosp_dropdown_col, md = 6)
  )
  ))
)


# This row has bottom two plots
bottom_left_plot = dbcRow(plot3)
bottom_right_plot = dbcRow(plot4)

col2_row5 <-  div(list(
  dbcRow(list(
    dbcCol(bottom_left_plot, md = 6),
    dbcCol(bottom_right_plot, md = 6)
  )
  )), 
  style=list("padding-bottom" = 0)
)



# This column has all plots and titles
col2 <- div(list(
  col2_row1,      # has titles of plots of first row
  col2_row2,      # has top two plots
  col2_row3,      # titles of bottom two plots
  col2_row4,      # has pace buttons and hospital dropdown
  col2_row5       # has bottom two plots
),
style=list('padding-left' = 0, "padding-right" = 0)
)


row3 <- div(
  dbcRow(
    list(
      dbcCol(col1, width = 3),
      dbcCol(col2, width = 9)
    )
  ),
  style=list('padding-top' = "30px")
)




######################################

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app %>% set_layout(list(
  row1,
  row2,
  row3
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

### 2nd map - callback
app$callback(
  output('map-bc', 'figure'),
  list(input('health_authority_buttons', 'value')),
  function(authority) {
    return(map(authority))
  }
)


### 3rd plot - callback
app$callback(
  output('procedure_plot3', 'figure'),
  list(input('health_authority_buttons', 'value'),
       input('year_slider', 'value'),
       input("pace_dropdown","value")),
  function(authority, year, pace) {
    return(procedure_plots(authority, year, pace))
  }
)

### 4th plot - callback
app$callback(
  output("wait_complete_plot4", "figure"),
  list(input("year_slider", "value"),
       input("hosp_drop", "value")),
  function(year, hospital) {
    return(wait_complete_plot(year, hospital))
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
app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))


