# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'http://cloud.r-project.org'
options(repos=r)

# ======================================================================

#we used these libraries, so will need to install all packages below...
# library(dash)
# library(dashHtmlComponents)
# library(dashCoreComponents)
# library(dashBootstrapComponents)

# library(devtools)
# library(ggplot2)
# library(plotly)

# library(tidyverse)
# library(readxl)
# library(hexbin)
# library(ggthemes)
# library(repr)
# library(lubridate)
# library(stringr)
# library(reshape2)
# library(gridExtra)



# packages go here
install.packages('remotes')
remotes::install_github('plotly/dashR', upgrade=TRUE)

install.packages('dash')
install.packages('dashHtmlComponents')
install.packages('dashCoreComponents')
#install.packages('dashBootstrapComponents')
install.packages('devtools')
install.packages('ggplot2')
install.packages('plotly')
install.packages('tidyverse')
install.packages('readxl')
install.packages('hexbin')
install.packages('ggthemes')
install.packages('repr')
install.packages('lubridate')
install.packages('stringr')
install.packages('reshape2')
install.packages('gridExtra')


install.packages("dplyr")
install.packages("usmap")

install.packages("BiocManager")
BiocManager::install("EBImage")



library(devtools)
remotes::install_github('facultyai/dash-bootstrap-components@r-release')

