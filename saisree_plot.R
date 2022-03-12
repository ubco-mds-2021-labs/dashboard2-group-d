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

###################    DATA MANIPULATION      ################################################################################################

path <- '/home/saisree/Desktop/Labs/Block5/551-visualization/milestone3/dashboard2-group-d/final_data_2015_2022.xlsx'
qdata <- read_excel(path)

qdata <- qdata[,-1]

print(qdata)
#convert <5 string to median value of 3
qdata <- mutate_if(qdata, is.character, str_replace_all, pattern = "<5", replacement = "3")

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

no_cataract <- main %>% 
  filter(procedure != 'Cataract Surgery')  

filtering_procedures <- function(ha_authority,yr,pace){
  no_cataract<-no_cataract %>% filter(health_authority==ha_authority,year>=yr[1], year<=yr[2]) 
  
  
  # by procedure group 
  procedure <- no_cataract %>% 
    group_by(procedure, year, quarter) %>% 
    summarise(wait_time_50 = mean(wait_time_50), wait_time_90 = mean(wait_time_90)) %>% 
    arrange(desc(wait_time_90))
  
  procedure_unite <- procedure %>% 
    unite(time, year, quarter, sep = "")
  
  procedure_order <- procedure_unite %>% group_by(procedure) %>% summarize(mean_wait_time_90_procedure=round(mean(wait_time_90),2)) %>% arrange(mean_wait_time_90_procedure) 
  fastest <- head(procedure_order,5)
  slowest <- tail(procedure_order,5)
  result  <- fastest
  if(pace=="Slowest"){
    result <- slowest
  }
  result
}

procedure_plots <- function(health_authority,year,pace){
  result <- filtering_procedures(health_authority,year,pace)
  
  pace_plot <- 
    ggplot(result, aes(y = procedure, x = mean_wait_time_90_procedure,fill=procedure))+ 
    geom_bar(stat="identity",show.legend=FALSE,alpha=0.6)+
    scale_fill_manual(values=c("steelblue3","gold3","black","navyblue","grey"))+
    geom_text(aes(label = mean_wait_time_90_procedure), hjust = 2,color="black")+
    labs(x = "Wait Time (weeks)",title="Fastest/Slowest Treated Procedures")+               
    theme_classic()+
    theme(axis.text=element_text(size=10),
          title=element_text(size=12),
          axis.title=element_text(size=10)       
          )
  pace_plot
}

app <- dash_app()

# Set the layout of the app

# year slider
yr_slider <- htmlDiv(
  list(
    dccRangeSlider(
      id="year_slider",
      min = 2015,
      max = 2021,
      marks=list(
        "2015" = "2015",
        "2016" = "2016",
        "2017" = "2017",
        "2018" = "2018",
        "2019" = "2019",
        "2020" = "2020",
        "2021" = "2021"
      ),
      step=1,
      value=list(2018,2021)
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

#health_authority buttons
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


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP) 

app %>% set_layout(list(
  yr_slider,
  ha_buttons,
  pace_button,
  dccGraph(id="procedure-graph")
)
)
app$callback(
  output('procedure-graph', 'figure'),
  list(input("health_authority_buttons","value"),input('year_slider', 'value'),input("pace_dropdown","value")),
  function(ha_authority="Interior",yr=c(2018,2021),pace="Fastest") {
    result <- filtering_procedures(ha_authority,yr,pace)
    pace_plot <- 
      ggplot(result, aes(y = procedure, x = mean_wait_time_90_procedure,fill=procedure))+ 
      geom_bar(stat="identity",show.legend=FALSE,alpha=0.6)+
      scale_fill_manual(values=c("steelblue3","gold3","black","navyblue","grey"))+
      geom_text(aes(label = mean_wait_time_90_procedure), hjust = 2,color="black")+
      labs(x = "Wait Time (weeks)",title="Fastest/Slowest Treated Procedures")+               
      theme_classic()+
      theme(axis.text=element_text(size=10),
            title=element_text(size=12),
            axis.title=element_text(size=10)       
      )
    ggplotly(pace_plot)
  }
)



app$run_server(debug = T)


