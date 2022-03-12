library(plotly)
library(dash)
library(tidyverse)
library(devtools)
library(dashCoreComponents)
library(dashHtmlComponents)
#library("dash-bootstrap-components")

#library(rjson)
library(sf)

#library(geojsonio)
#library(sp)

#path <- './data/HA_2018.geojson'
#map_json <- fromJSON(file = path)
#spdf <- geojson_read(path,  what = "sp")
#st_file <- st_read(path, quiet = TRUE)

map_data <- st_read('./data/convert.json', quiet = TRUE)

new <- ggplot(st_geometry(map_data)) + geom_sf()+
  theme_void()


abc <- ggplotly(new)


app <- dash_app()

app %>% set_layout(list(
  dccGraph(figure = abc),
  dccGraph((figure = abc1))
)
)

app %>% run_app()



