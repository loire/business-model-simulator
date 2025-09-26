# UI for Business Model Simulation Dashboard

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Business Model Simulator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulation", tabName = "simulation", icon = icon("chart-line")),
      menuItem("Variables", tabName = "variables", icon = icon("cogs")),
      menuItem("Results", tabName = "results", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Simulation Tab
      tabItem(
        tabName = "simulation",
        fluidRow(
          # Input Panel
          box(
            title = "Simulation Parameters", status = "primary", solidHeader = TRUE,
            width = 4, height = "600px",
            h4("Facility Settings"),
            numericInput("max_customers", "Max Customers (Facility Size):",
              value = 100, min = 10, max = 1000, step = 10
            ),
            h4("Income Parameters"),
            numericInput("entry_fee", "Entry Fee (€):",
              value = 15, min = 0, step = 0.50
            ),
            numericInput("entry_fee_sd", "Entry Fee Std Dev (€):",
              value = 2, min = 0, step = 0.10
            ),
            numericInput("consumables_mean", "Consumables Mean (Poisson λ):",
              value = 1, min = 0.1, max = 10, step = 0.1
            ),
            numericInput("consumables_price", "Consumables Price per Item (€):",
              value = 3, min = 0, step = 0.10
            ),
            numericInput("consumables_price_sd", "Consumables Price Std Dev (€):",
              value = 0.5, min = 0, step = 0.05
            ),
            h4("Customer Flow"),
            numericInput("customers_mean", "Average Customers per Day:",
              value = 50, min = 1, max = 1000, step = 1
            ),
            numericInput("customers_sd", "Customers Std Dev:",
              value = 15, min = 0, step = 1
            ),
            br(),
            actionButton("run_simulation", "Run Simulation",
              class = "btn-primary btn-lg", width = "100%"
            )
          ),

          # Results Panel
          box(
            title = "Simulation Results", status = "success", solidHeader = TRUE,
            width = 8, height = "600px",
            tabsetPanel(
              tabPanel(
                "Customer Flow",
                plotlyOutput("customers_plot", height = "500px")
              ),
              tabPanel(
                "Cash Flow",
                plotlyOutput("cashflow_plot", height = "500px")
              )
            )
          )
        ),

        # Summary Statistics
        fluidRow(
          valueBoxOutput("avg_daily_customers"),
          valueBoxOutput("avg_daily_revenue"),
          valueBoxOutput("total_profit")
        )
      ),

      # Variables Tab
      tabItem(
        tabName = "variables",
        fluidRow(
          box(
            title = "Expense Variables", status = "warning", solidHeader = TRUE,
            width = 12,
            h4("Default Expense Categories"),

            # Rent
            fluidRow(
              column(3, h5("Rent (Monthly)")),
              column(3, numericInput("rent_value", "Amount (€):", value = 2000, min = 0, step = 50)),
              column(3, numericInput("rent_sd", "Std Dev (€):", value = 200, min = 0, step = 10)),
              column(3, selectInput("rent_freq", "Frequency:", choices = list("Monthly" = "monthly"), selected = "monthly"))
            ),

            # Taxes
            fluidRow(
              column(3, h5("Taxes (Yearly)")),
              column(3, numericInput("taxes_value", "Amount (€):", value = 5000, min = 0, step = 100)),
              column(3, numericInput("taxes_sd", "Std Dev (€):", value = 500, min = 0, step = 50)),
              column(3, selectInput("taxes_freq", "Frequency:", choices = list("Yearly" = "yearly"), selected = "yearly"))
            ),

            # Consumables
            fluidRow(
              column(3, h5("Consumables (Weekly)")),
              column(3, numericInput("consumables_cost_value", "Amount (€):", value = 300, min = 0, step = 25)),
              column(3, numericInput("consumables_cost_sd", "Std Dev (€):", value = 50, min = 0, step = 5)),
              column(3, selectInput("consumables_cost_freq", "Frequency:", choices = list("Weekly" = "weekly"), selected = "weekly"))
            ),

            # IT
            fluidRow(
              column(3, h5("IT (Yearly)")),
              column(3, numericInput("it_value", "Amount (€):", value = 2000, min = 0, step = 100)),
              column(3, numericInput("it_sd", "Std Dev (€):", value = 300, min = 0, step = 25)),
              column(3, selectInput("it_freq", "Frequency:", choices = list("Yearly" = "yearly"), selected = "yearly"))
            ),

            # Material (Investment)
            fluidRow(
              column(3, h5("Material (Investment)")),
              column(3, numericInput("material_value", "Amount (€):", value = 15000, min = 0, step = 500)),
              column(3, numericInput("material_sd", "Std Dev (€):", value = 2000, min = 0, step = 100)),
              column(3, selectInput("material_freq", "Frequency:", choices = list("Investment" = "investment"), selected = "investment"))
            ),

            # Staff
            fluidRow(
              column(3, h5("Staff (Monthly)")),
              column(3, numericInput("staff_value", "Amount (€):", value = 4000, min = 0, step = 200)),
              column(3, numericInput("staff_sd", "Std Dev (€):", value = 400, min = 0, step = 50)),
              column(3, selectInput("staff_freq", "Frequency:", choices = list("Monthly" = "monthly"), selected = "monthly"))
            )
          )
        )
      ),

      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = "Detailed Results", status = "info", solidHeader = TRUE,
            width = 12,
            DT::dataTableOutput("results_table")
          )
        )
      )
    )
  )
)
