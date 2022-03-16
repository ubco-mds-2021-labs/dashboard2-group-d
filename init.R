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
# 
# library(devtools)
# library(ggplot2)
# library(plotly)
# 
# library(reshape2)
# library(tidyverse)



# packages go here
install.packages('remotes')
install.packages('dash')
install.packages('dashHtmlComponents')
install.packages('dashCoreComponents')
install.packages('dashBootstrapComponents')
install.packages('devtools')
install.packages('ggplot2')
install.packages('plotly')
install.packages('tidyverse')
install.packages('reshape2')


remotes::install_github('plotly/dashR', upgrade=TRUE)
remotes::install_github('facultyai/dash-bootstrap-components@r-release')

