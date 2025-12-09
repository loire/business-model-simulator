# Business Model Simulation Dashboard
# A Shiny application for simulating business model scenarios


# Source modules
source("helpers/defaults.R")
source("helpers/helper.R")
source("ui.R")
source("server.R")

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)


# Run the application
shinyApp(ui = ui, server = server)
