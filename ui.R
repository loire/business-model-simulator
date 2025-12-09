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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Revenue Items", tabName = "revenue", icon = icon("coins")),
      menuItem("Expenses", tabName = "expenses", icon = icon("receipt")),
      menuItem("Customer Patterns", tabName = "seasonal", icon = icon("calendar-alt")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    # Add CSS and JavaScript for tooltips
    tags$head(
      tags$style(HTML("
        .help-icon {
          font-size: 14px;
        }
        .tooltip-inner {
          max-width: 300px;
          text-align: left;
        }
      ")),
      tags$script(HTML("
        $(document).ready(function(){
          $('[data-toggle=\"tooltip\"]').tooltip({
            html: true,
            container: 'body'
          });
        });
      "))
    ),
    tabItems(
      # Dashboard Tab
      tabItem(
        tabName = "dashboard",

        # Loading status notification
        uiOutput("loading_notification"),
        fluidRow(
          # Control Panel
          box(
            title = "Simulation Control", status = "primary", solidHeader = TRUE,
            width = 4,
            h4("Basic Settings"),
            create_slider_with_input("max_customers", "Facility Capacity",
              min = SLIDER_RANGES$max_customers$min,
              max = SLIDER_RANGES$max_customers$max,
              value = DEFAULT_FACILITY$max_customers,
              step = SLIDER_RANGES$max_customers$step,
              help_key = "facility_capacity"
            ),
            create_slider_with_input("base_customers", "Base Daily Customers",
              min = SLIDER_RANGES$customers$min,
              max = SLIDER_RANGES$customers$max,
              value = DEFAULT_FACILITY$base_customers,
              step = SLIDER_RANGES$customers$step,
              help_key = "base_customers"
            ),
            create_slider_with_input("customers_sd", "Customer Variability",
              min = SLIDER_RANGES$customers_sd$min,
              max = SLIDER_RANGES$customers_sd$max,
              value = DEFAULT_FACILITY$customers_sd,
              step = SLIDER_RANGES$customers_sd$step,
              help_key = "customer_variability"
            ),
            h4("Simulation Parameters"),
            div(
              tags$label(
                tagList("Currency", create_help_icon("currency")),
                `for` = "currency"
              ),
              selectInput("currency", NULL,
                choices = setNames(
                  names(CURRENCIES),
                  sapply(CURRENCIES, function(x) paste(x$name, paste0("(", x$symbol, ")")))
                ),
                selected = DEFAULT_CURRENCY
              )
            ),
            div(
              tags$label(
                tagList("Number of Iterations", create_help_icon("n_iterations")),
                `for` = "n_iterations"
              ),
              numericInput("n_iterations", NULL,
                value = DEFAULT_SIMULATION$n_iterations, min = 1, max = 1000, step = 1
              )
            ),
            div(
              tags$label(
                tagList("Simulation Years", create_help_icon("n_years")),
                `for` = "n_years"
              ),
              numericInput("n_years", NULL,
                value = 3, min = 1, max = 10, step = 1
              )
            ),
            br(),
            actionButton("run_simulation", "Run Simulation",
              class = "btn-primary btn-lg", width = "100%"
            ),
            br(), br(),
            h5("Data Management"),
            fluidRow(
              column(
                6,
                downloadButton("download_parameters", "Save Parameters",
                  class = "btn-info btn-sm", style = "width: 100%"
                )
              ),
              column(
                6,
                downloadButton("download_results", "Save Results",
                  class = "btn-success btn-sm", style = "width: 100%"
                )
              )
            ),
            br(),
            fluidRow(
              column(
                12,
                fileInput("upload_parameters", "Load Parameters (.csv)",
                  accept = ".csv", width = "100%"
                )
              )
            )
          ),

          # Results Panel
          box(
            title = "Simulation Results", status = "success", solidHeader = TRUE,
            width = 8,
            div(
              tags$h4(
                tagList("Time Range Selection", create_help_icon("time_range_controls"))
              ),
              uiOutput("time_range_slider")
            ),
            br(),
            tabsetPanel(
              tabPanel(
                tagList("Customer Flow", create_help_icon("customer_flow_graph")),
                plotlyOutput("customers_plot", height = "400px")
              ),
              tabPanel(
                tagList("Cash Flow", create_help_icon("cashflow_graph")),
                plotlyOutput("cashflow_plot", height = "400px")
              ),
              tabPanel(
                tagList("Monthly Profit/Loss", create_help_icon("monthly_profit_graph")),
                plotlyOutput("monthly_profit_plot", height = "400px")
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

      # Customer Flow Tab
      tabItem(
        tabName = "customers",
        fluidRow(
          box(
            title = "Weekly Pattern", status = "info", solidHeader = TRUE,
            width = 6,
            h4("Day-of-Week Multipliers"),
            p("Adjust customer flow for each day of the week (1.0 = average):"),
            lapply(1:7, function(i) {
              day_name <- DAYS_OF_WEEK[i]
              sliderInput(paste0("weekly_", tolower(day_name)),
                paste(day_name, ":"),
                min = SLIDER_RANGES$weekly_multiplier$min,
                max = SLIDER_RANGES$weekly_multiplier$max,
                value = DEFAULT_WEEKLY_MULTIPLIERS[i],
                step = SLIDER_RANGES$weekly_multiplier$step
              )
            })
          ),
          box(
            title = "Growth Trend", status = "warning", solidHeader = TRUE,
            width = 6,
            h4("Business Growth/Decline"),
            checkboxInput("trend_enabled", "Enable Growth Trend", value = FALSE),
            conditionalPanel(
              condition = "input.trend_enabled",
              sliderInput("annual_growth_rate", "Annual Growth Rate:",
                min = SLIDER_RANGES$trend_rate$min,
                max = SLIDER_RANGES$trend_rate$max,
                value = DEFAULT_TREND$annual_rate,
                step = SLIDER_RANGES$trend_rate$step
              ),
              p("Positive values = growth, negative = decline")
            )
          )
        )
      ),

      # Revenue Items Tab
      tabItem(
        tabName = "revenue",
        fluidRow(
          box(
            title = "Revenue Items Configuration", status = "success", solidHeader = TRUE,
            width = 12,

            # Rerun simulation button
            fluidRow(
              column(
                12,
                actionButton("rerun_simulation_revenue", "🚀 Rerun Simulation",
                  class = "btn-primary btn-lg",
                  style = "margin-bottom: 15px;"
                ),
                hr()
              )
            ),
            h4("Configure Revenue Sources"),

            # Dynamic UI for revenue items will be generated in server
            uiOutput("revenue_items_ui"),
            br(),
            h4("Add New Revenue Item"),
            fluidRow(
              column(
                3,
                textInput("new_revenue_name", "Item Name:", placeholder = "e.g., Gift Shop")
              ),
              column(
                3,
                selectInput("new_revenue_type", "Type:",
                  choices = REVENUE_TYPES,
                  selected = "fixed_per_customer"
                )
              ),
              column(
                2,
                uiOutput("new_revenue_price_ui")
              ),
              column(
                2,
                uiOutput("new_revenue_price_sd_ui")
              ),
              column(
                2,
                br(),
                actionButton("add_revenue_item", "Add Item", class = "btn-success")
              )
            ),

            # Additional parameters for different revenue types
            conditionalPanel(
              condition = "input.new_revenue_type == 'poisson_per_customer'",
              numericInput("new_revenue_lambda", "Poisson Lambda:", value = 1, min = 0.1)
            ),
            conditionalPanel(
              condition = "input.new_revenue_type == 'probability_per_customer'",
              fluidRow(
                column(
                  6,
                  sliderInput("new_revenue_probability", "Purchase Probability:",
                    min = 0, max = 1, value = 0.3, step = 0.01
                  )
                ),
                column(
                  6,
                  numericInput("new_revenue_lambda_prob", "Quantity Lambda:", value = 1, min = 0.1)
                )
              )
            )
          )
        )
      ),

      # Expenses Tab
      tabItem(
        tabName = "expenses",
        fluidRow(
          box(
            title = "Expense Management", status = "warning", solidHeader = TRUE,
            width = 12,

            # Rerun simulation button
            fluidRow(
              column(
                12,
                actionButton("rerun_simulation_expenses", "🚀 Rerun Simulation",
                  class = "btn-primary btn-lg",
                  style = "margin-bottom: 15px;"
                ),
                hr()
              )
            ),

            # Dynamic UI for expenses will be generated in server
            uiOutput("expenses_ui"),
            br(),
            h4("Add New Expense Category"),
            fluidRow(
              column(
                3,
                div(
                  tags$label("Expense Name:", `for` = "new_expense_name"),
                  textInput("new_expense_name", NULL, placeholder = "e.g., Insurance")
                )
              ),
              column(
                2,
                uiOutput("new_expense_value_ui")
              ),
              column(
                2,
                uiOutput("new_expense_sd_ui")
              ),
              column(
                3,
                selectInput("new_expense_freq", "Frequency:",
                  choices = FREQUENCY_OPTIONS,
                  selected = "monthly"
                )
              ),
              column(
                2,
                br(),
                actionButton("add_expense", "Add Expense", class = "btn-warning")
              )
            ),
            textAreaInput("new_expense_description", "Description:",
              placeholder = "Brief description of this expense category",
              rows = 2
            )
          )
        )
      ),

      # Customer Flow & Seasonal Patterns Tab
      tabItem(
        tabName = "seasonal",
        # Weekly Pattern Section
        fluidRow(
          box(
            title = "Weekly Pattern", status = "info", solidHeader = TRUE,
            width = 6,
            h4("Day-of-Week Multipliers"),
            p("Adjust customer flow for each day of the week (1.0 = average):"),
            lapply(1:7, function(i) {
              day_name <- DAYS_OF_WEEK[i]
              sliderInput(paste0("weekly_", tolower(day_name)),
                paste(day_name, ":"),
                min = SLIDER_RANGES$weekly_multiplier$min,
                max = SLIDER_RANGES$weekly_multiplier$max,
                value = DEFAULT_WEEKLY_MULTIPLIERS[i],
                step = SLIDER_RANGES$weekly_multiplier$step
              )
            })
          ),
          box(
            title = "Growth Trend", status = "warning", solidHeader = TRUE,
            width = 6,
            h4("Business Growth/Decline"),
            checkboxInput("trend_enabled", "Enable Growth Trend", value = FALSE),
            conditionalPanel(
              condition = "input.trend_enabled",
              sliderInput("annual_growth_rate", "Annual Growth Rate:",
                min = SLIDER_RANGES$trend_rate$min,
                max = SLIDER_RANGES$trend_rate$max,
                value = DEFAULT_TREND$annual_rate,
                step = SLIDER_RANGES$trend_rate$step
              ),
              p("Positive values = growth, negative = decline")
            )
          )
        ),

        # Seasonal Periods Section
        fluidRow(
          box(
            title = "Seasonal Periods", status = "success", solidHeader = TRUE,
            width = 12,
            h4("Special Periods & Events"),
            p("Define periods with different customer multipliers (holidays, events, etc.)"),

            # Dynamic UI for seasonal periods
            uiOutput("seasonal_periods_ui"),
            br(),

            # Summary for seasonal patterns
            fluidRow(
              column(
                12,
                h5("Seasonal Periods Configuration"),
                p("Configure special periods that affect customer flow throughout the year.")
              )
            ),
            br(),
            h4("Add New Seasonal Period"),
            fluidRow(
              column(
                3,
                textInput("new_period_name", "Period Name:", placeholder = "e.g., Easter Week")
              ),
              column(
                2,
                selectInput("new_period_recurrence", "Recurrence:",
                  choices = RECURRENCE_TYPES,
                  selected = "yearly"
                )
              ),
              column(
                2,
                conditionalPanel(
                  condition = "input.new_period_recurrence == 'once'",
                  dateInput("new_period_start_date", "Start Date:", value = Sys.Date())
                ),
                conditionalPanel(
                  condition = "input.new_period_recurrence == 'yearly'",
                  textInput("new_period_start_md", "Start (MM-DD):",
                    value = format(Sys.Date(), "%m-%d"), placeholder = "12-25"
                  )
                )
              ),
              column(
                2,
                conditionalPanel(
                  condition = "input.new_period_recurrence == 'once'",
                  dateInput("new_period_end_date", "End Date:", value = Sys.Date() + 7)
                ),
                conditionalPanel(
                  condition = "input.new_period_recurrence == 'yearly'",
                  textInput("new_period_end_md", "End (MM-DD):",
                    value = format(Sys.Date() + 7, "%m-%d"), placeholder = "12-31"
                  )
                )
              ),
              column(
                2,
                sliderInput("new_period_multiplier", "Multiplier:",
                  min = SLIDER_RANGES$seasonal_multiplier$min,
                  max = SLIDER_RANGES$seasonal_multiplier$max,
                  value = 1.0, step = SLIDER_RANGES$seasonal_multiplier$step
                )
              ),
              column(
                1,
                br(),
                actionButton("add_seasonal_period", "Add Period", class = "btn-info")
              )
            ),
            textAreaInput("new_period_description", "Description:",
              placeholder = "Description of this seasonal period",
              rows = 2
            )
          )
        )
      ),

      # Results Tab
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            title = tagList("Simulation Summary", create_help_icon("avg_daily_customers")),
            status = "primary", solidHeader = TRUE,
            width = 6,
            h4(tagList("Key Metrics", create_help_icon("avg_daily_revenue"))),
            DT::dataTableOutput("summary_table")
          ),
          box(
            title = "Parameter Overview", status = "info", solidHeader = TRUE,
            width = 6,
            h4("Current Configuration"),
            verbatimTextOutput("params_summary")
          )
        ),
        fluidRow(
          box(
            title = tagList("Detailed Monthly Results", create_help_icon("total_profit")),
            status = "success", solidHeader = TRUE,
            width = 12,
            p("Comprehensive monthly breakdown of business performance including customer flow, revenue, and profitability analysis."),
            DT::dataTableOutput("results_table")
          )
        )
      )
    )
  )
)
