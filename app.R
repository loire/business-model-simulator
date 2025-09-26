# Business Model Simulation Dashboard
# A Shiny application for simulating business model scenarios

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

# Source modules
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
