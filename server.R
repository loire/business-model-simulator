# Server logic for Business Model Simulation Dashboard

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)
library(data.table)

# Install and load calheatmapR if not available
if (!require("calheatmapR", quietly = TRUE)) {
  if (!require("remotes", quietly = TRUE)) {
    install.packages("remotes")
    library(remotes)
  }
  remotes::install_github("durtal/calheatmapR")
  library(calheatmapR)
} else {
  library(calheatmapR)
}

# Load the modern simulation engine
source("simulation_engine.R")

# Define server logic
server <- function(input, output, session) {
  # Enhanced tabular data structures
  business_tables <- reactiveValues(
    parameters_table = NULL, # Daily parameters (reactive to UI changes)
    simulation_results = NULL, # Simulation results after running
    aggregated_results = NULL, # Processed results for visualization
    summary_statistics = NULL # Summary stats
  )

  # Configuration data (unchanged structure for compatibility)
  config_data <- reactiveValues(
    revenue_items = DEFAULT_REVENUE_ITEMS,
    expenses = DEFAULT_EXPENSES,
    seasonal_periods = DEFAULT_SEASONAL_PERIODS,
    current_currency = DEFAULT_CURRENCY
  )

  # Load latest simulation results if available
  tryCatch(
    {
      if (file.exists("latest_simulation.csv") && file.exists("latest_parameters.csv")) {
        cat("Loading latest simulation results...\n")

        # Load the results
        loaded_results <- load_results_table("latest_simulation.csv")
        loaded_parameters <- load_parameters_table("latest_parameters.csv")

        # Process the results for visualization
        aggregated <- aggregate_simulation_results(loaded_results)

        # Update business_tables with loaded data
        business_tables$simulation_results <- loaded_results
        business_tables$parameters_table <- loaded_parameters
        business_tables$aggregated_results <- aggregated$daily_summary
        business_tables$summary_statistics <- aggregated$summary_stats

        # Update loading status
        loading_status$data_loaded <- TRUE
        loading_status$load_message <- paste(
          "Loaded previous simulation:",
          nrow(loaded_results), "results from",
          format(min(loaded_results$date), "%Y-%m-%d"), "to",
          format(max(loaded_results$date), "%Y-%m-%d")
        )
        loading_status$load_time <- Sys.time()

        cat(
          "Successfully loaded", nrow(loaded_results), "simulation rows and",
          nrow(loaded_parameters), "parameter rows\n"
        )
      } else {
        loading_status$data_loaded <- FALSE
        loading_status$load_message <- "No previous simulation found - run a new simulation"
      }
    },
    error = function(e) {
      cat("Could not load latest results:", e$message, "\n")
      cat("Starting with fresh simulation data\n")
      loading_status$data_loaded <- FALSE
      loading_status$load_message <- paste("Could not load previous data:", e$message)
    }
  )

  # Synchronize slider and numeric inputs with debouncing to prevent loops
  sync_values <- reactiveValues(
    updating_max_customers = FALSE,
    updating_base_customers = FALSE,
    updating_customers_sd = FALSE
  )

  # Loading status for UI notifications
  loading_status <- reactiveValues(
    data_loaded = FALSE,
    load_message = "",
    load_time = Sys.time()
  )

  # Max customers synchronization
  observeEvent(input$max_customers_num, {
    if (!sync_values$updating_max_customers && !is.null(input$max_customers_num)) {
      if (is.null(input$max_customers) || abs(input$max_customers - input$max_customers_num) > 0.1) {
        sync_values$updating_max_customers <- TRUE
        updateSliderInput(session, "max_customers", value = input$max_customers_num)
        sync_values$updating_max_customers <- FALSE
      }
    }
  })

  observeEvent(input$max_customers, {
    if (!sync_values$updating_max_customers && !is.null(input$max_customers)) {
      if (is.null(input$max_customers_num) || abs(input$max_customers_num - input$max_customers) > 0.1) {
        sync_values$updating_max_customers <- TRUE
        updateNumericInput(session, "max_customers_num", value = input$max_customers)
        sync_values$updating_max_customers <- FALSE
      }
    }
  })

  # Base customers synchronization
  observeEvent(input$base_customers_num, {
    if (!sync_values$updating_base_customers && !is.null(input$base_customers_num)) {
      if (is.null(input$base_customers) || abs(input$base_customers - input$base_customers_num) > 0.1) {
        sync_values$updating_base_customers <- TRUE
        updateSliderInput(session, "base_customers", value = input$base_customers_num)
        sync_values$updating_base_customers <- FALSE
      }
    }
  })

  observeEvent(input$base_customers, {
    if (!sync_values$updating_base_customers && !is.null(input$base_customers)) {
      if (is.null(input$base_customers_num) || abs(input$base_customers_num - input$base_customers) > 0.1) {
        sync_values$updating_base_customers <- TRUE
        updateNumericInput(session, "base_customers_num", value = input$base_customers)
        sync_values$updating_base_customers <- FALSE
      }
    }
  })

  # Customer SD synchronization
  observeEvent(input$customers_sd_num, {
    if (!sync_values$updating_customers_sd && !is.null(input$customers_sd_num)) {
      if (is.null(input$customers_sd) || abs(input$customers_sd - input$customers_sd_num) > 0.1) {
        sync_values$updating_customers_sd <- TRUE
        updateSliderInput(session, "customers_sd", value = input$customers_sd_num)
        sync_values$updating_customers_sd <- FALSE
      }
    }
  })

  observeEvent(input$customers_sd, {
    if (!sync_values$updating_customers_sd && !is.null(input$customers_sd)) {
      if (is.null(input$customers_sd_num) || abs(input$customers_sd_num - input$customers_sd) > 0.1) {
        sync_values$updating_customers_sd <- TRUE
        updateNumericInput(session, "customers_sd_num", value = input$customers_sd)
        sync_values$updating_customers_sd <- FALSE
      }
    }
  })

  # Reactive for selected time range
  time_range <- reactiveValues(
    start = NULL,
    end = NULL
  )

  # Build simulation parameters from UI inputs (legacy structure for compatibility)
  # Unified reactive parameters system - updates when any input changes
  reactive_parameters <- reactive({
    # Collect weekly multipliers
    weekly_multipliers <- c(
      input$weekly_monday %||% DEFAULT_WEEKLY_MULTIPLIERS[1],
      input$weekly_tuesday %||% DEFAULT_WEEKLY_MULTIPLIERS[2],
      input$weekly_wednesday %||% DEFAULT_WEEKLY_MULTIPLIERS[3],
      input$weekly_thursday %||% DEFAULT_WEEKLY_MULTIPLIERS[4],
      input$weekly_friday %||% DEFAULT_WEEKLY_MULTIPLIERS[5],
      input$weekly_saturday %||% DEFAULT_WEEKLY_MULTIPLIERS[6],
      input$weekly_sunday %||% DEFAULT_WEEKLY_MULTIPLIERS[7]
    )

    # Build trend parameters
    trend_params <- list(
      enabled = input$trend_enabled %||% FALSE,
      annual_rate = input$annual_growth_rate %||% 0.05,
      start_date = Sys.Date()
    )

    # Build investment timing parameters - collect dates for each investment expense
    investment_dates <- list()
    for (exp_name in names(config_data$expenses)) {
      expense <- config_data$expenses[[exp_name]]
      if (expense$freq == "investment") {
        date_input_id <- paste0("exp_", exp_name, "_date")
        # Default investment date to 30 days after simulation start
        default_investment_date <- Sys.Date() + 30
        investment_dates[[exp_name]] <- input[[date_input_id]] %||% default_investment_date
      }
    }

    # Build complete parameter list
    params <- list(
      n_iterations = input$n_iterations %||% DEFAULT_SIMULATION$n_iterations,
      n_days = (input$n_years %||% 3) * 365,
      start_date = Sys.Date(),
      max_customers = input$max_customers %||% DEFAULT_FACILITY$max_customers,
      base_customers = input$base_customers %||% DEFAULT_FACILITY$base_customers,
      customers_sd = input$customers_sd %||% DEFAULT_FACILITY$customers_sd,
      weekly_multipliers = weekly_multipliers,
      seasonal_periods = config_data$seasonal_periods,
      trend_params = trend_params,
      investment_dates = investment_dates,
      expenses = config_data$expenses,
      revenue_items = config_data$revenue_items
    )

    # Create parameters table
    param_dt <- create_complete_parameters_table(params)

    # Store in business_tables for access elsewhere
    business_tables$parameters_table <- param_dt

    # Return both params and table for flexibility
    return(list(
      params = params,
      table = param_dt,
      weekly_multipliers = weekly_multipliers
    ))
  })

  # Simulation wrapper using tabular approach
  run_modern_business_simulation <- function() {
    # Get current parameters from unified reactive
    params_data <- reactive_parameters()
    param_dt <- params_data$table
    params <- params_data$params

    # Validate parameters (basic checks)
    if (nrow(param_dt) == 0) {
      stop("Parameters table is empty")
    }
    if (params$n_iterations < 1) {
      stop("Number of iterations must be at least 1")
    }

    # Run simulation
    results_dt <- run_modern_simulation(
      param_dt = param_dt,
      revenue_items = config_data$revenue_items,
      expenses = config_data$expenses,
      n_iterations = params$n_iterations
    )

    # Aggregate results for visualization
    aggregated <- aggregate_simulation_results(results_dt)

    return(list(
      raw_results = results_dt,
      daily_summary = aggregated$daily_summary,
      summary_stats = aggregated$summary_stats
    ))
  }

  # Dynamic UI for new revenue price inputs
  output$new_revenue_price_ui <- renderUI({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    ranges <- get_currency_adjusted_ranges(input$currency %||% DEFAULT_CURRENCY)

    numericInput("new_revenue_price",
      paste0("Price (", currency_symbol, "):"),
      value = if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) 400 else 10,
      min = 0,
      max = ranges$price$max,
      step = ranges$price$step
    )
  })

  output$new_revenue_price_sd_ui <- renderUI({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    ranges <- get_currency_adjusted_ranges(input$currency %||% DEFAULT_CURRENCY)

    numericInput("new_revenue_price_sd",
      paste0("Price Std Dev (", currency_symbol, "):"),
      value = 0, min = 0,
      max = ranges$price_sd$max,
      step = ranges$price_sd$step
    )
  })

  # Dynamic UI for new expense inputs
  output$new_expense_value_ui <- renderUI({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    ranges <- get_currency_adjusted_ranges(input$currency %||% DEFAULT_CURRENCY)

    sliderInput("new_expense_value",
      paste0("Amount (", currency_symbol, "):"),
      min = ranges$expense_value$min,
      max = ranges$expense_value$max,
      value = if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) 26500 else 1000,
      step = ranges$expense_value$step
    )
  })

  output$new_expense_sd_ui <- renderUI({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    ranges <- get_currency_adjusted_ranges(input$currency %||% DEFAULT_CURRENCY)

    sliderInput("new_expense_sd",
      paste0("Std Dev (", currency_symbol, "):"),
      min = ranges$expense_sd$min,
      max = ranges$expense_sd$max,
      value = 0,
      step = ranges$expense_sd$step
    )
  })

  # Loading status notification
  output$loading_notification <- renderUI({
    if (loading_status$data_loaded) {
      div(
        class = "alert alert-success",
        style = "margin: 10px 0;",
        tags$strong("✓ Data Loaded: "),
        loading_status$load_message
      )
    } else if (loading_status$load_message != "") {
      div(
        class = "alert alert-info",
        style = "margin: 10px 0;",
        tags$strong("ℹ Info: "),
        loading_status$load_message
      )
    }
  })

  # Dynamic UI for revenue items
  output$revenue_items_ui <- renderUI({
    items <- config_data$revenue_items

    if (length(items) == 0) {
      return(p("No revenue items configured. Add one below."))
    }

    item_boxes <- lapply(names(items), function(item_name) {
      item <- items[[item_name]]
      fluidRow(
        column(2, h5(item$name)),
        column(2, p(paste("Type:", item$type))),
        column(
          2,
          numericInput(paste0("rev_", item_name, "_price"),
            paste0("Price (", get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY), "):"),
            value = item$price, min = 0, step = 0.1
          )
        ),
        column(
          2,
          numericInput(paste0("rev_", item_name, "_price_sd"),
            paste0("Price SD (", get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY), "):"),
            value = item$price_sd, min = 0, step = 0.01
          )
        ),
        column(
          2,
          if (item$type == "poisson_per_customer") {
            numericInput(paste0("rev_", item_name, "_lambda"), "Lambda:",
              value = item$lambda, min = 0.1, step = 0.1
            )
          } else if (item$type == "probability_per_customer") {
            sliderInput(paste0("rev_", item_name, "_prob"), "Probability:",
              min = 0, max = 1, value = item$probability, step = 0.01
            )
          }
        ),
        column(
          2,
          actionButton(paste0("del_rev_", item_name), "Remove",
            class = "btn-danger btn-sm"
          )
        )
      )
    })

    do.call(tagList, item_boxes)
  })

  # Dynamic UI for expenses
  output$expenses_ui <- renderUI({
    expenses <- config_data$expenses

    if (length(expenses) == 0) {
      return(p("No expenses configured. Add one below."))
    }

    expense_boxes <- lapply(names(expenses), function(exp_name) {
      expense <- expenses[[exp_name]]
      is_investment <- expense$freq == "investment"

      # Create the main row
      main_row <- fluidRow(
        column(2, h5(expense$name)),
        column(
          2,
          div(
            tags$label(paste0("Amount (", get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY), ")"),
              `for` = paste0("exp_", exp_name, "_value")
            ),
            sliderInput(paste0("exp_", exp_name, "_value"), NULL,
              min = 0, max = max(50000, expense$value * 2), value = expense$value, step = 100
            )
          )
        ),
        column(
          2,
          div(
            tags$label(paste0("Std Dev (", get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY), ")"),
              `for` = paste0("exp_", exp_name, "_sd")
            ),
            sliderInput(paste0("exp_", exp_name, "_sd"), NULL,
              min = 0, max = max(5000, expense$sd * 2), value = expense$sd, step = 50
            )
          )
        ),
        column(
          2,
          selectInput(paste0("exp_", exp_name, "_freq"), "Frequency:",
            choices = FREQUENCY_OPTIONS, selected = expense$freq
          )
        ),
        column(2, p(expense$description, style = "font-size: 12px;")),
        column(
          2,
          actionButton(paste0("del_exp_", exp_name), "Remove",
            class = "btn-danger btn-sm"
          )
        )
      )

      # Add date picker row for investments
      if (is_investment) {
        date_row <- fluidRow(
          column(2, ""),
          column(
            4,
            div(
              tags$label(paste0("Investment Date for ", expense$name),
                `for` = paste0("exp_", exp_name, "_date")
              ),
              dateInput(paste0("exp_", exp_name, "_date"), NULL,
                value = Sys.Date() + 30, # Default to 30 days after simulation start
                min = Sys.Date() + 1, # Minimum 1 day after simulation start
                max = as.Date("2030-12-31")
              )
            )
          ),
          column(6, p("Investment date must be after simulation start date",
            style = "font-size: 11px; color: #666; margin-top: 5px;"
          ))
        )
        return(tagList(main_row, date_row))
      } else {
        return(main_row)
      }
    })

    do.call(tagList, expense_boxes)
  })

  # Dynamic UI for seasonal periods
  output$seasonal_periods_ui <- renderUI({
    periods <- config_data$seasonal_periods

    if (length(periods) == 0) {
      return(p("No seasonal periods configured. Add one below."))
    }

    period_boxes <- lapply(1:length(periods), function(i) {
      period <- periods[[i]]
      fluidRow(
        column(2, h5(period$name)),
        column(2, p(paste("Start:", period$start))),
        column(2, p(paste("End:", period$end))),
        column(
          2,
          sliderInput(paste0("seasonal_", i, "_mult"), "Multiplier:",
            min = 0.1, max = 5.0, value = period$multiplier, step = 0.1
          )
        ),
        column(2, p(period$description, style = "font-size: 12px;")),
        column(
          2,
          actionButton(paste0("del_seasonal_", i), "Remove",
            class = "btn-danger btn-sm"
          )
        )
      )
    })

    do.call(tagList, period_boxes)
  })

  # Debounced calendar data reactive to prevent excessive updates
  calendar_data_reactive <- debounce(reactive({
    # Get unified parameters (triggers on all input changes)
    params_data <- reactive_parameters()

    # Also depend on seasonal periods changes
    seasonal_periods <- config_data$seasonal_periods

    # Get weekly multipliers from unified reactive
    weekly_mults <- params_data$weekly_multipliers

    # Create a year's worth of dates (2025 as reference year)
    start_date <- as.Date("2025-01-01")
    end_date <- as.Date("2025-12-31")
    all_dates <- seq(start_date, end_date, by = "day")

    # Calculate combined multipliers for each day (seasonal * weekly)
    combined_multipliers <- sapply(all_dates, function(date) {
      # Get seasonal multiplier (uses config_data$seasonal_periods which can change)
      seasonal_mult <- calculate_seasonal_multiplier(date, seasonal_periods)

      # Get day of week multiplier - convert Sunday=1 to Monday=1 indexing
      day_of_week <- lubridate::wday(date) # Sunday = 1
      weekly_mult <- weekly_mults[ifelse(day_of_week == 1, 7, day_of_week - 1)]

      # Combined effect
      return(seasonal_mult * weekly_mult)
    })

    # Create data in the format expected by calheatmapR
    calendar_data <- as.list(combined_multipliers)
    names(calendar_data) <- as.numeric(as.POSIXct(all_dates))

    # Debug info
    cat("Calendar reactive updated - Range:", min(combined_multipliers), "to", max(combined_multipliers), "\n")

    return(list(
      data = calendar_data,
      multipliers = combined_multipliers,
      dates = all_dates,
      min_val = min(combined_multipliers),
      max_val = max(combined_multipliers),
      timestamp = Sys.time() # Add timestamp to ensure proper invalidation
    ))
  }), millis = 500) # Debounce for 500ms to prevent rapid updates

  # Enhanced Calendar Visualization that reacts to input changes
  output$seasonal_calendar <- renderCalheatmapR({
    # Get reactive calendar data
    cal_data <- calendar_data_reactive()

    # Clear any existing calendar instances and reset positioning
    session$sendCustomMessage(type = "clearCalendar", message = list())

    # Small delay to ensure clearing is complete before rendering
    Sys.sleep(0.1)

    # Create proper diverging color scale centered on 1.0
    min_val <- cal_data$min_val
    max_val <- cal_data$max_val
    mid_val <- 1.0 # Neutral point (no multiplier effect)

    # Use actual data range for proper mapping
    # Create 5 evenly spaced legend points across the actual data range
    legend_values <- seq(min_val, max_val, length.out = 5)
    legend_values <- round(legend_values, 2)

    # Create diverging colors that map to actual data
    # Find where 1.0 falls in the data range to center the diverging scale
    neutral_position <- (mid_val - min_val) / (max_val - min_val)

    if (neutral_position < 0.2) {
      # Mostly above neutral - use more reds
      diverging_colors <- c("#1e40af", "#3b82f6", "#f59e0b", "#ef4444", "#dc2626")
    } else if (neutral_position > 0.8) {
      # Mostly below neutral - use more blues
      diverging_colors <- c("#1e40af", "#3b82f6", "#60a5fa", "#93c5fd", "#dbeafe")
    } else {
      # Balanced around neutral - true diverging
      diverging_colors <- c("#1e40af", "#3b82f6", "#f8fafc", "#f87171", "#dc2626")
    }

    # Create the calendar heatmap with forced positioning and size constraints
    cal_widget <- calheatmapR(
      data = cal_data$data,
      width = 800, # Fixed width
      height = 400 # Fixed height
    ) %>%
      chDomain(
        domain = "month",
        subDomain = "day",
        start = "2025-01-01",
        range = 12,
        cellSize = 15, # Fixed cell size
        cellPadding = 2 # Fixed padding
      ) %>%
      chLabel(position = "top") %>%
      chLegend(
        display = TRUE,
        orientation = "horizontal",
        legend = legend_values,
        colours = diverging_colors,
        cellSize = 15, # Match domain cell size
        margin = c(10, 10, 10, 10) # Fixed margins
      ) # Return the widget
    cal_widget
  })

  # Calendar summary table
  output$calendar_summary <- DT::renderDataTable({
    # Get calendar data from unified reactive
    cal_data <- calendar_data_reactive()

    # Create summary by month using the same dates and multipliers
    start_date <- as.Date("2025-01-01")
    end_date <- as.Date("2025-12-31")
    all_dates <- cal_data$dates
    all_multipliers <- cal_data$multipliers

    # Calculate monthly summaries using existing multipliers
    monthly_summary <- data.frame(
      Month = month.name,
      Min_Multiplier = numeric(12),
      Max_Multiplier = numeric(12),
      Avg_Multiplier = numeric(12)
    )

    for (month in 1:12) {
      month_indices <- which(lubridate::month(all_dates) == month)
      month_multipliers <- all_multipliers[month_indices]

      monthly_summary$Min_Multiplier[month] <- round(min(month_multipliers), 2)
      monthly_summary$Max_Multiplier[month] <- round(max(month_multipliers), 2)
      monthly_summary$Avg_Multiplier[month] <- round(mean(month_multipliers), 2)
    }

    DT::datatable(monthly_summary,
      options = list(pageLength = 12, dom = "t", scrollY = "160px"),
      rownames = FALSE
    ) %>%
      DT::formatRound(columns = c("Min_Multiplier", "Max_Multiplier", "Avg_Multiplier"), digits = 2)
  })

  # Calendar statistics text
  output$calendar_stats <- renderText({
    # Get calendar data from unified reactive
    cal_data <- calendar_data_reactive()

    all_dates <- cal_data$dates
    all_multipliers <- cal_data$multipliers

    # Find peak and low days
    peak_idx <- which.max(all_multipliers)
    low_idx <- which.min(all_multipliers)

    paste0(
      "Peak Period:\n",
      format(all_dates[peak_idx], "%B %d, %Y (%A)"), "\n",
      "Multiplier: ", round(all_multipliers[peak_idx], 2), "\n\n",
      "Lowest Period:\n",
      format(all_dates[low_idx], "%B %d, %Y (%A)"), "\n",
      "Multiplier: ", round(all_multipliers[low_idx], 2), "\n\n",
      "Overall Range: ", round(min(all_multipliers), 2), " to ", round(max(all_multipliers), 2), "\n",
      "Average: ", round(mean(all_multipliers), 2)
    )
  })

  # Time range slider UI using enhanced data structure
  output$time_range_slider <- renderUI({
    if (is.null(business_tables$aggregated_results)) {
      return(p("Run simulation to enable time range selection"))
    }

    dates <- business_tables$aggregated_results$date
    min_date <- min(dates)
    max_date <- max(dates)

    dateRangeInput("time_range",
      "Select Time Range for Charts:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date
    )
  })

  # Currency conversion observer
  observeEvent(input$currency, {
    if (!is.null(input$currency) && input$currency != config_data$current_currency) {
      old_currency <- config_data$current_currency
      new_currency <- input$currency

      # Convert revenue items
      config_data$revenue_items <- convert_monetary_list(
        config_data$revenue_items, old_currency, new_currency
      )

      # Convert expenses
      config_data$expenses <- convert_monetary_list(
        config_data$expenses, old_currency, new_currency
      )

      # Update current currency
      config_data$current_currency <- new_currency

      # Get currency-adjusted ranges
      new_ranges <- get_currency_adjusted_ranges(new_currency)

      # Update slider ranges for new expense inputs
      updateSliderInput(session, "new_expense_value",
        min = new_ranges$expense_value$min,
        max = new_ranges$expense_value$max,
        step = new_ranges$expense_value$step
      )

      updateSliderInput(session, "new_expense_sd",
        min = new_ranges$expense_sd$min,
        max = new_ranges$expense_sd$max,
        step = new_ranges$expense_sd$step
      )

      showNotification(
        paste("Currency converted from", old_currency, "to", new_currency),
        type = "message"
      )

      # Force UI refresh to update sliders with new ranges
      # This triggers the reactive UI elements to re-render with proper ranges
      session$sendCustomMessage("currency_changed", list(currency = new_currency))
    }
  })

  # Add new revenue item
  observeEvent(input$add_revenue_item, {
    req(input$new_revenue_name, input$new_revenue_type)

    item_id <- gsub("[^a-zA-Z0-9]", "_", tolower(input$new_revenue_name))

    # Convert VND display values to actual values for storage
    price_value <- input$new_revenue_price %||% 10
    price_sd_value <- input$new_revenue_price_sd %||% 0

    if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) {
      price_value <- format_vnd_actual(price_value)
      price_sd_value <- format_vnd_actual(price_sd_value)
    }

    new_item <- list(
      name = input$new_revenue_name,
      type = input$new_revenue_type,
      price = price_value,
      price_sd = price_sd_value
    )

    if (input$new_revenue_type == "poisson_per_customer") {
      new_item$lambda <- input$new_revenue_lambda %||% 1
    } else if (input$new_revenue_type == "probability_per_customer") {
      new_item$probability <- input$new_revenue_probability %||% 0.3
      new_item$lambda <- input$new_revenue_lambda_prob %||% 1
    }

    config_data$revenue_items[[item_id]] <- new_item

    # Clear inputs
    updateTextInput(session, "new_revenue_name", value = "")
    updateNumericInput(session, "new_revenue_price", value = 10)
    updateNumericInput(session, "new_revenue_price_sd", value = 0)
  })

  # Add new expense
  observeEvent(input$add_expense, {
    req(input$new_expense_name)

    expense_id <- gsub("[^a-zA-Z0-9]", "_", tolower(input$new_expense_name))

    # Convert VND display values to actual values for storage
    expense_value <- input$new_expense_value %||% 1000
    expense_sd <- input$new_expense_sd %||% 0 # Changed default to 0

    if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) {
      expense_value <- format_vnd_actual(expense_value)
      expense_sd <- format_vnd_actual(expense_sd)
    }

    new_expense <- list(
      name = input$new_expense_name,
      value = expense_value,
      sd = expense_sd,
      freq = input$new_expense_freq %||% "monthly",
      description = input$new_expense_description %||% ""
    )

    config_data$expenses[[expense_id]] <- new_expense

    # Clear inputs
    updateTextInput(session, "new_expense_name", value = "")
    updateTextAreaInput(session, "new_expense_description", value = "")
  })

  # Add new seasonal period
  observeEvent(input$add_seasonal_period, {
    req(input$new_period_name, input$new_period_recurrence)

    recurrence <- input$new_period_recurrence %||% "yearly"

    if (recurrence == "once") {
      req(input$new_period_start_date, input$new_period_end_date)
      start_val <- as.character(input$new_period_start_date)
      end_val <- as.character(input$new_period_end_date)
    } else {
      req(input$new_period_start_md, input$new_period_end_md)
      start_val <- input$new_period_start_md
      end_val <- input$new_period_end_md
    }

    new_period <- list(
      name = input$new_period_name,
      start = start_val,
      end = end_val,
      multiplier = input$new_period_multiplier %||% 1.0,
      recurring = recurrence,
      description = input$new_period_description %||% ""
    )

    config_data$seasonal_periods[[length(config_data$seasonal_periods) + 1]] <- new_period

    # Clear inputs
    updateTextInput(session, "new_period_name", value = "")
    updateTextAreaInput(session, "new_period_description", value = "")
  })

  # Observer to update seasonal period multipliers when changed
  observe({
    # Create dynamic observers for seasonal period multipliers
    if (length(config_data$seasonal_periods) > 0) {
      for (i in 1:length(config_data$seasonal_periods)) {
        local({
          idx <- i
          multiplier_input <- paste0("seasonal_", idx, "_mult")

          # Check if this input exists and update the seasonal period
          if (!is.null(input[[multiplier_input]])) {
            config_data$seasonal_periods[[idx]]$multiplier <- input[[multiplier_input]]
          }
        })
      }
    }
  })

  # Run simulation when button is clicked
  observeEvent(input$run_simulation, {
    cat("\n*** SIMULATION STARTED ***\n")

    withProgress(message = "Running simulation...", value = 0, {
      incProgress(0.1, detail = "Building parameters table")

      tryCatch(
        {
          # Run the modern simulation
          incProgress(0.3, detail = "Running Monte Carlo simulation")
          results <- run_modern_business_simulation()

          incProgress(0.8, detail = "Processing and aggregating results")

          # Store results in modern format
          business_tables$simulation_results <- results$raw_results
          business_tables$aggregated_results <- results$daily_summary
          business_tables$summary_statistics <- results$summary_stats

          # Also save to files for persistence
          incProgress(0.9, detail = "Saving results to files")
          save_parameters_table(business_tables$parameters_table, "latest_parameters.csv")
          save_results_table(business_tables$simulation_results, "latest_simulation.csv")

          incProgress(1.0, detail = "Complete!")

          showNotification("Simulation completed and saved successfully!")
        },
        error = function(e) {
          cat("Simulation error:", e$message, "\n")
          showNotification(paste("Simulation error:", e$message), type = "error")
        }
      )
    })
  })

  # Rerun simulation from Revenue tab
  observeEvent(input$rerun_simulation_revenue, {
    cat("\n*** SIMULATION RESTARTED FROM REVENUE TAB ***\n")

    withProgress(message = "Rerunning simulation...", value = 0, {
      incProgress(0.1, detail = "Building parameters table")

      tryCatch(
        {
          # Run the modern simulation
          incProgress(0.3, detail = "Running Monte Carlo simulation")
          results <- run_modern_business_simulation()

          incProgress(0.8, detail = "Processing and aggregating results")

          # Store results in modern format
          business_tables$simulation_results <- results$raw_results
          business_tables$aggregated_results <- results$daily_summary
          business_tables$summary_statistics <- results$summary_stats

          # Also save to files for persistence
          incProgress(0.9, detail = "Saving results to files")
          save_parameters_table(business_tables$parameters_table, "latest_parameters.csv")
          save_results_table(business_tables$simulation_results, "latest_simulation.csv")

          incProgress(1.0, detail = "Complete!")

          showNotification("Simulation rerun completed successfully from Revenue tab!", type = "success")
        },
        error = function(e) {
          cat("Simulation error:", e$message, "\n")
          showNotification(paste("Simulation error:", e$message), type = "error")
        }
      )
    })
  })

  # Rerun simulation from Expenses tab
  observeEvent(input$rerun_simulation_expenses, {
    cat("\n*** SIMULATION RESTARTED FROM EXPENSES TAB ***\n")

    withProgress(message = "Rerunning simulation...", value = 0, {
      incProgress(0.1, detail = "Building parameters table")

      tryCatch(
        {
          # Run the modern simulation
          incProgress(0.3, detail = "Running Monte Carlo simulation")
          results <- run_modern_business_simulation()

          incProgress(0.8, detail = "Processing and aggregating results")

          # Store results in modern format
          business_tables$simulation_results <- results$raw_results
          business_tables$aggregated_results <- results$daily_summary
          business_tables$summary_statistics <- results$summary_stats

          # Also save to files for persistence
          incProgress(0.9, detail = "Saving results to files")
          save_parameters_table(business_tables$parameters_table, "latest_parameters.csv")
          save_results_table(business_tables$simulation_results, "latest_simulation.csv")

          incProgress(1.0, detail = "Complete!")

          showNotification("Simulation rerun completed successfully from Expenses tab!", type = "success")
        },
        error = function(e) {
          cat("Simulation error:", e$message, "\n")
          showNotification(paste("Simulation error:", e$message), type = "error")
        }
      )
    })
  })

  # Customer plot using enhanced data structure
  output$customers_plot <- renderPlotly({
    if (is.null(business_tables$aggregated_results)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Run simulation to see results",
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Customers per Day"),
          title = "Customer Flow Over Time",
          showlegend = FALSE
        )
      return(p)
    }

    # Prepare plot data using modern approach
    df <- prepare_plot_data(business_tables$aggregated_results, "customers")

    # Apply time range filter if available
    if (!is.null(input$time_range)) {
      df <- df[df$Date >= input$time_range[1] & df$Date <= input$time_range[2], ]
    }

    p <- plot_ly() %>%
      add_ribbons(
        data = df, x = ~Date,
        ymin = ~Q25, ymax = ~Q75,
        fillcolor = "rgba(0,100,200,0.2)",
        line = list(color = "rgba(255,255,255,0)"),
        name = "25%-75% Range",
        showlegend = TRUE
      ) %>%
      add_lines(
        data = df, x = ~Date, y = ~Mean,
        line = list(color = "blue", width = 2),
        name = "Mean",
        mode = "lines"
      ) %>%
      layout(
        title = "Customer Flow Over Time",
        xaxis = list(
          title = "Date",
          rangeslider = list(visible = TRUE),
          rangeselector = list(
            buttons = list(
              list(count = 30, label = "30D", step = "day", stepmode = "backward"),
              list(count = 90, label = "3M", step = "day", stepmode = "backward"),
              list(count = 180, label = "6M", step = "day", stepmode = "backward"),
              list(count = 1, label = "1Y", step = "year", stepmode = "backward"),
              list(step = "all", label = "All")
            )
          )
        ),
        yaxis = list(title = "Customers per Day"),
        hovermode = "x unified"
      )

    p
  })

  # Cash flow plot using enhanced data structure
  output$cashflow_plot <- renderPlotly({
    if (is.null(business_tables$aggregated_results)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Run simulation to see results",
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Cumulative Cash Flow"),
          title = "Cash Flow Over Time",
          showlegend = FALSE
        )
      return(p)
    }

    # Prepare plot data using modern approach
    df <- prepare_plot_data(business_tables$aggregated_results, "cashflow")
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    currency_abbr <- if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) "K-VND" else (input$currency %||% DEFAULT_CURRENCY)

    # Apply time range filter if available
    if (!is.null(input$time_range)) {
      df <- df[df$Date >= input$time_range[1] & df$Date <= input$time_range[2], ]
    }

    # Create the plot showing all cumulative cash flow values (including negative)
    p <- plot_ly() %>%
      add_ribbons(
        data = df, x = ~Date,
        ymin = ~Q25, ymax = ~Q75,
        fillcolor = "rgba(200,100,0,0.2)",
        line = list(color = "rgba(255,255,255,0)"),
        name = "25%-75% Range",
        showlegend = TRUE
      ) %>%
      add_lines(
        data = df, x = ~Date, y = ~Mean,
        line = list(color = "orange", width = 2),
        name = "Mean Cumulative Cash Flow",
        mode = "lines"
      ) %>%
      # Add a horizontal reference line at zero
      add_trace(
        x = c(min(df$Date), max(df$Date)),
        y = c(0, 0),
        type = "scatter",
        mode = "lines",
        line = list(color = "gray", width = 1, dash = "dot"),
        name = "Zero Line",
        showlegend = FALSE
      )

    p <- p %>%
      layout(
        title = "Cumulative Cash Flow Over Time",
        xaxis = list(
          title = "Date",
          rangeslider = list(visible = TRUE),
          rangeselector = list(
            buttons = list(
              list(count = 30, label = "30D", step = "day", stepmode = "backward"),
              list(count = 90, label = "3M", step = "day", stepmode = "backward"),
              list(count = 180, label = "6M", step = "day", stepmode = "backward"),
              list(count = 1, label = "1Y", step = "year", stepmode = "backward"),
              list(step = "all", label = "All")
            )
          )
        ),
        yaxis = list(
          title = paste("Cumulative Cash Flow", paste0("(", currency_abbr, ")")),
          zeroline = TRUE,
          zerolinecolor = "rgb(200,200,200)",
          zerolinewidth = 2
        ),
        hovermode = "x unified",
        showlegend = TRUE
      )

    p
  })

  # Monthly Profit/Loss plot using enhanced data structure
  output$monthly_profit_plot <- renderPlotly({
    if (is.null(business_tables$simulation_results)) {
      p <- plot_ly() %>%
        add_annotations(
          text = "Run simulation to see results",
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.5,
          showarrow = FALSE,
          font = list(size = 16)
        ) %>%
        layout(
          xaxis = list(title = "Month"),
          yaxis = list(title = "Amount"),
          title = "Monthly Revenue vs Expenses",
          showlegend = FALSE
        )
      return(p)
    }

    # Aggregate monthly data
    monthly_data <- aggregate_monthly_results(business_tables$simulation_results)
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    currency_abbr <- if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) "K-VND" else (input$currency %||% DEFAULT_CURRENCY)

    # Apply VND scaling if needed
    if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) {
      monthly_data[, `:=`(
        mean_revenue = format_vnd_display(mean_revenue),
        q25_revenue = format_vnd_display(q25_revenue),
        q75_revenue = format_vnd_display(q75_revenue),
        mean_expenses = format_vnd_display(mean_expenses),
        q25_expenses = format_vnd_display(q25_expenses),
        q75_expenses = format_vnd_display(q75_expenses),
        mean_profit = format_vnd_display(mean_profit),
        q25_profit = format_vnd_display(q25_profit),
        q75_profit = format_vnd_display(q75_profit)
      )]
    }

    p <- plot_ly()

    # Add revenue data
    p <- p %>%
      add_ribbons(
        data = monthly_data, x = ~month_start,
        ymin = ~q25_revenue, ymax = ~q75_revenue,
        fillcolor = "rgba(0,150,0,0.2)",
        line = list(color = "rgba(255,255,255,0)"),
        name = "Revenue Range (25%-75%)",
        showlegend = TRUE
      ) %>%
      add_lines(
        data = monthly_data, x = ~month_start, y = ~mean_revenue,
        line = list(color = "green", width = 3),
        name = "Mean Revenue",
        mode = "lines"
      )

    # Add expense data
    p <- p %>%
      add_ribbons(
        data = monthly_data, x = ~month_start,
        ymin = ~q25_expenses, ymax = ~q75_expenses,
        fillcolor = "rgba(200,0,0,0.2)",
        line = list(color = "rgba(255,255,255,0)"),
        name = "Expense Range (25%-75%)",
        showlegend = TRUE
      ) %>%
      add_lines(
        data = monthly_data, x = ~month_start, y = ~mean_expenses,
        line = list(color = "red", width = 3),
        name = "Mean Expenses",
        mode = "lines"
      )

    # Add profit/loss bars
    p <- p %>%
      add_bars(
        data = monthly_data, x = ~month_start, y = ~mean_profit,
        marker = list(
          color = ifelse(monthly_data$mean_profit >= 0, "rgba(0,150,0,0.6)", "rgba(200,0,0,0.6)"),
          line = list(
            color = ifelse(monthly_data$mean_profit >= 0, "green", "red"),
            width = 1
          )
        ),
        name = "Monthly Profit/Loss",
        yaxis = "y2"
      )

    # Add zero line for profit/loss
    p <- p %>%
      add_segments(
        x = min(monthly_data$month_start), xend = max(monthly_data$month_start),
        y = 0, yend = 0,
        line = list(color = "black", width = 1, dash = "dash"),
        yaxis = "y2",
        name = "Break Even",
        showlegend = FALSE
      )

    p <- p %>%
      layout(
        title = "Monthly Financial Overview",
        xaxis = list(
          title = "Month",
          type = "date"
        ),
        yaxis = list(
          title = paste("Revenue & Expenses", paste0("(", currency_abbr, ")")),
          side = "left"
        ),
        yaxis2 = list(
          title = paste("Profit/Loss", paste0("(", currency_abbr, ")")),
          side = "right",
          overlaying = "y",
          showgrid = FALSE
        ),
        hovermode = "x unified",
        legend = list(x = 0, y = 1),
        showlegend = TRUE
      )

    p
  })

  # Summary value boxes
  output$avg_daily_customers <- renderValueBox({
    value <- if (is.null(business_tables$summary_statistics)) "Run Simulation" else round(business_tables$summary_statistics$avg_daily_customers, 1)
    valueBox(
      value = value,
      subtitle = "Avg Daily Customers",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$avg_daily_revenue <- renderValueBox({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    currency_abbr <- if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) "K-VND" else (input$currency %||% DEFAULT_CURRENCY)
    value <- if (is.null(business_tables$summary_statistics)) "Run Simulation" else paste0(currency_symbol, format(business_tables$summary_statistics$avg_daily_revenue, big.mark = ","))
    valueBox(
      value = value,
      subtitle = paste("Avg Daily Revenue", paste0("(", currency_abbr, ")")),
      icon = icon("coins"),
      color = "green"
    )
  })

  output$total_profit <- renderValueBox({
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    currency_abbr <- if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) "K-VND" else (input$currency %||% DEFAULT_CURRENCY)
    value <- if (is.null(business_tables$summary_statistics)) "Run Simulation" else paste0(currency_symbol, format(business_tables$summary_statistics$final_profit, big.mark = ","))
    box_color <- if (is.null(business_tables$summary_statistics)) {
      "yellow"
    } else if (business_tables$summary_statistics$final_profit >= 0) {
      "green"
    } else {
      "red"
    }
    valueBox(
      value = value,
      subtitle = paste("3-Year Profit", paste0("(", currency_abbr, ")")),
      icon = icon("chart-line"),
      color = box_color
    )
  })

  # Results table using aggregated data
  output$results_table <- DT::renderDataTable({
    if (is.null(business_tables$aggregated_results)) {
      return(data.frame(Message = "Run simulation to see detailed results"))
    }

    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)
    currency_abbr <- if (is_vnd_currency(input$currency %||% DEFAULT_CURRENCY)) "K-VND" else (input$currency %||% DEFAULT_CURRENCY)

    # Use aggregated results directly (already in data.table format)
    daily_data <- business_tables$aggregated_results

    # Create monthly summary from daily data
    monthly_summary <- daily_data[, .(
      Month = format(date, "%Y-%m"),
      date = date,
      avg_customers = mean_customers,
      min_customers = q25_customers,
      max_customers = q75_customers,
      avg_revenue = mean_revenue,
      cumulative_cashflow = mean_cashflow
    )][, .(
      Avg_Customers = round(mean(avg_customers), 1),
      Min_Customers = round(min(min_customers), 1),
      Max_Customers = round(max(max_customers), 1),
      Total_Customers = round(sum(avg_customers), 0),
      Avg_Monthly_Revenue = round(mean(avg_revenue), 0),
      End_Balance = round(last(cumulative_cashflow), 0),
      Monthly_Cash_Change = round(last(cumulative_cashflow) - first(cumulative_cashflow), 0)
    ), by = Month]

    # Convert back to data.frame for DT compatibility
    monthly_df <- as.data.frame(monthly_summary)

    # Create formatted columns
    monthly_df$Revenue_per_Customer <- round(ifelse(monthly_df$Total_Customers > 0,
      monthly_df$Avg_Monthly_Revenue / monthly_df$Total_Customers * 30, 0 # Approximate monthly
    ), 2)

    monthly_df$Profitability <- ifelse(monthly_df$Monthly_Cash_Change > 0, "Profitable",
      ifelse(monthly_df$Monthly_Cash_Change < 0, "Loss", "Break-even")
    )

    # Create display columns
    monthly_df$Customer_Range_Text <- paste(monthly_df$Min_Customers, "-", monthly_df$Max_Customers)
    monthly_df$Cash_Flow_Change_Text <- paste0(currency_symbol, format(monthly_df$Monthly_Cash_Change, big.mark = ","))
    monthly_df$Cumulative_Balance_Text <- paste0(currency_symbol, format(monthly_df$End_Balance, big.mark = ","))
    monthly_df$Revenue_per_Customer_Text <- paste0(currency_symbol, monthly_df$Revenue_per_Customer)

    # Select final columns
    final_summary <- data.frame(
      Month = monthly_df$Month,
      `Avg Daily Customers` = monthly_df$Avg_Customers,
      `Customer Range` = monthly_df$Customer_Range_Text,
      `Monthly Customers` = monthly_df$Total_Customers,
      `Cash Flow Change` = monthly_df$Cash_Flow_Change_Text,
      `Cumulative Balance` = monthly_df$Cumulative_Balance_Text,
      `Revenue/Customer` = monthly_df$Revenue_per_Customer_Text,
      Status = monthly_df$Profitability,
      stringsAsFactors = FALSE
    )

    DT::datatable(final_summary,
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        columnDefs = list(
          list(className = "dt-center", targets = 1:7),
          list(width = "100px", targets = 0),
          list(width = "80px", targets = 1:3),
          list(width = "120px", targets = 4:6)
        )
      ),
      rownames = FALSE,
      caption = paste("Monthly Business Analysis (", currency_abbr, ")")
    ) %>%
      DT::formatStyle(
        "Status",
        backgroundColor = DT::styleEqual(
          c("Profitable", "Loss", "Break-even"),
          c("#d4edda", "#f8d7da", "#fff3cd")
        )
      ) %>%
      DT::formatStyle(columns = 1:8, fontSize = "11px")
  })

  # Summary table using new data structure
  output$summary_table <- DT::renderDataTable({
    if (is.null(business_tables$summary_statistics)) {
      return(data.frame(Metric = "No results", Value = "Run simulation"))
    }

    stats <- business_tables$summary_statistics
    currency_symbol <- get_currency_display_symbol(input$currency %||% DEFAULT_CURRENCY)

    summary_df <- data.frame(
      Metric = c("Average Daily Customers", "Average Daily Revenue", "3-Year Profit"),
      Value = c(
        paste(round(stats$avg_daily_customers, 1), "customers"),
        paste0(currency_symbol, format(round(stats$avg_daily_revenue), big.mark = ",")),
        paste0(currency_symbol, format(round(stats$final_profit), big.mark = ","))
      )
    )

    DT::datatable(summary_df, options = list(dom = "t"), rownames = FALSE)
  })

  # Parameters summary
  output$params_summary <- renderText({
    if (is.null(input$max_customers)) {
      return("Configure parameters and run simulation")
    }

    paste(
      "Facility Capacity:", input$max_customers %||% "N/A", "customers\n",
      "Base Daily Customers:", input$base_customers %||% "N/A", "\n",
      "Customer Variability:", input$customers_sd %||% "N/A", "\n",
      "Simulation Years:", input$n_years %||% "N/A", "\n",
      "Iterations:", input$n_iterations %||% "N/A", "\n",
      "Revenue Items:", length(config_data$revenue_items), "\n",
      "Expense Categories:", length(config_data$expenses), "\n",
      "Seasonal Periods:", length(config_data$seasonal_periods), "\n",
      "Parameters Table Rows:", if (is.null(business_tables$parameters_table)) 0 else nrow(business_tables$parameters_table), "\n",
      "Simulation Results Rows:", if (is.null(business_tables$simulation_results)) 0 else nrow(business_tables$simulation_results)
    )
  })

  # Download handlers for tabular data
  output$download_parameters <- downloadHandler(
    filename = function() {
      paste0("business_parameters_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(business_tables$parameters_table)) {
        # Create parameters table if not exists
        params_data <- reactive_parameters()
        param_dt <- params_data$table
      } else {
        param_dt <- business_tables$parameters_table
      }
      fwrite(param_dt, file)
    }
  )

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("simulation_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (is.null(business_tables$simulation_results)) {
        stop("No simulation results to download. Run simulation first.")
      }
      fwrite(business_tables$simulation_results, file)
    }
  )

  # Upload handler for parameters
  observeEvent(input$upload_parameters, {
    req(input$upload_parameters)

    tryCatch(
      {
        # Read the uploaded file
        uploaded_params <- fread(input$upload_parameters$datapath)

        # Basic validation
        required_cols <- c("date", "max_customers", "base_customers", "expected_customers")
        missing_cols <- setdiff(required_cols, names(uploaded_params))

        if (length(missing_cols) > 0) {
          showNotification(
            paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
            type = "error"
          )
          return()
        }

        # Convert date column
        uploaded_params[, date := as.Date(date)]

        # Store the parameters table
        business_tables$parameters_table <- uploaded_params

        showNotification("Parameters loaded successfully!", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error loading parameters:", e$message), type = "error")
      }
    )
  })
}
