library(plotly)
library(dash)
library(tidyverse)
library(devtools)
library(dashCoreComponents)
library(dashHtmlComponents)
library(OpenImageR)
#library(png)
#library(patchwork)  
#library(base64enc)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

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



###### Map data and plot #########################
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


#################################################
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
    dbcRow(div("completed_cases_card")),
    br(),
    dbcRow(div("wait_cases_card")),
    br(),
    dbcRow(div("wait_50_card")),
    br(),
    dbcRow(div("wait_90_card"))
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
top_left_plot <- dbcRow(div("proportion_cases plot here"))
top_right_plot <- dbcRow(dccGraph(id = "map-bc"))

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
pace_buttons_col <- dbcRow(div("fast_slow_button here"))
hosp_dropdown_col <- dbcRow(div("hosp_dropdown"))

col2_row4 <- div(list(
  dbcRow(list(
    dbcCol(pace_buttons_col, md = 6),
    dbcCol(hosp_dropdown_col, md = 6)
  )
))
)


# This row has bottom two plots
bottom_left_plot = dbcRow(div("procedure_plot here"))
bottom_right_plot = dbcRow(div("hosp_wait_comp_cases here"))

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





app %>% set_layout(list(
  row1,
  row2,
  row3
)
)
  




####### Call back for map ###################################
app$callback(
  output('map-bc', 'figure'),
  list(input("health_authority_buttons","value")),
  function(authority = "Interior"){
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
        xaxis=list(showticklabels=FALSE, zerolinecolor = '#ffff', showgrid = F),
        yaxis=list(showticklabels=FALSE, zerolinecolor = '#ffff', showgrid = F),
        xaxis = list(showgrid = FALSE),
        yaxis = list(showgrid = FALSE)
      )
    return(map_plot)
  }
)


app %>% run_app()

