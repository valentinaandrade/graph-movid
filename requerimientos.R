if (!require("remotes")) install.packages("remotes")

if (!require("highcharter")) remotes::install_github("jbkunst/highcharter")

if (!require("bs4Dash")) remotes::install_github("RinteRface/bs4Dash", force = TRUE)

# CRAN
paquetes <- c("shiny", "tidyverse", "shinyWidgets", "ggsci")

if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("ggsci")) install.packages("ggsci")
if (!require("zoo")) install.packages("zoo")
if (!require("lubridate")) install.packages("lubridate")

# for(paquetes in paquetes) {
# 
#   if (!require(paquetes)) install.packages(paquetes)
# 
# }
