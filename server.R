# Server logic for Business Model Simulation Dashboard

library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lubridate)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store simulation results
  simulation_data <- reactiveValues(
    customers_ts = NULL,
    cashflow_ts = NULL,
    summary_stats = NULL
  )

  # Function to convert frequency to daily multiplier
  freq_to_daily <- function(frequency) {
    switch(frequency,
      "daily" = 1,
      "weekly" = 1 / 7,
      "monthly" = 1 / 30.44, # Average days per month
      "yearly" = 1 / 365.25 # Average days per year
    )
  }

  # Function to calculate daily expenses
  calculate_daily_expenses <- function() {
    expenses <- list(
      rent = list(value = input$rent_value, sd = input$rent_sd, freq = input$rent_freq),
      taxes = list(value = input$taxes_value, sd = input$taxes_sd, freq = input$taxes_freq),
      consumables_cost = list(value = input$consumables_cost_value, sd = input$consumables_cost_sd, freq = input$consumables_cost_freq),
      it = list(value = input$it_value, sd = input$it_sd, freq = input$it_freq),
      staff = list(value = input$staff_value, sd = input$staff_sd, freq = input$staff_freq)
    )

    daily_expenses <- 0

    for (expense_name in names(expenses)) {
      expense <- expenses[[expense_name]]
      if (expense$freq != "investment") {
        daily_multiplier <- freq_to_daily(expense$freq)
        daily_expense <- expense$value * daily_multiplier
        daily_expenses <- daily_expenses + daily_expense
      }
    }

    return(daily_expenses)
  }

  # Function to calculate one-time investment
  calculate_investment <- function() {
    return(input$material_value)
  }

  # Main simulation function
  run_business_simulation <- function() {
    cat("=== SIMULATION START ===\n")
    start_time <- Sys.time()

    # Simulation parameters (reduced for testing)
    n_iterations <- 10 # Reduced from 100 for debugging
    n_days <- 365 # Reduced from 1095 for debugging
    start_date <- Sys.Date()
    dates <- seq(start_date, start_date + n_days - 1, by = "day")

    cat("Simulation parameters:\n")
    cat("- Iterations:", n_iterations, "\n")
    cat("- Days:", n_days, "\n")
    cat("- Date range:", as.character(start_date), "to", as.character(start_date + n_days - 1), "\n")

    # Initialize result matrices
    cat("Initializing matrices...\n")
    customers_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)
    cashflow_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)
    cat("Matrices created: customers_matrix", dim(customers_matrix), "cashflow_matrix", dim(cashflow_matrix), "\n")

    # Calculate fixed daily expenses
    cat("Calculating expenses...\n")
    daily_expenses <- calculate_daily_expenses()
    investment <- calculate_investment()
    cat("Daily expenses:", daily_expenses, "Investment:", investment, "\n")

    # Run simulations
    cat("Starting simulation loops...\n")
    for (iter in 1:n_iterations) {
      if (iter %% 10 == 0 || iter <= 5) {
        cat("Iteration", iter, "of", n_iterations, "\n")
      }
      cumulative_cashflow <- -investment # Start with negative investment

      for (day in 1:n_days) {
        if (iter == 1 && (day %% 100 == 0 || day <= 10)) {
          cat("  Day", day, "of", n_days, "in iteration 1\n")
        }
        # Generate number of customers (normal distribution, bounded by facility size)
        customers_raw <- rnorm(1, mean = input$customers_mean, sd = input$customers_sd)
        customers <- max(0, min(round(customers_raw), input$max_customers))
        customers_matrix[day, iter] <- customers

        if (iter == 1 && day <= 3) {
          cat("    Day", day, ": customers_raw =", customers_raw, ", customers =", customers, "\n")
        }

        # Calculate daily revenue
        # Entry fees
        entry_fees <- customers * rnorm(1, mean = input$entry_fee, sd = input$entry_fee_sd)

        # Consumables revenue (Poisson distribution per customer)
        consumables_revenue <- 0
        if (customers > 0) {
          # Optimize: vectorize the consumables calculation
          consumables_counts <- rpois(customers, lambda = input$consumables_mean)
          consumables_prices <- rnorm(customers, mean = input$consumables_price, sd = input$consumables_price_sd)
          consumables_revenue <- sum(consumables_counts * consumables_prices)
        }

        if (iter == 1 && day <= 3) {
          cat("    Day", day, ": entry_fees =", entry_fees, ", consumables_revenue =", consumables_revenue, "\n")
        }

        # Total daily revenue
        daily_revenue <- max(0, entry_fees) + max(0, consumables_revenue)

        # Daily profit/loss
        daily_profit <- daily_revenue - daily_expenses
        cumulative_cashflow <- cumulative_cashflow + daily_profit

        cashflow_matrix[day, iter] <- cumulative_cashflow

        if (iter == 1 && day <= 3) {
          cat("    Day", day, ": daily_revenue =", daily_revenue, ", daily_profit =", daily_profit, ", cumulative =", cumulative_cashflow, "\n")
        }
      }

      if (iter %% 25 == 0) {
        cat("Completed iteration", iter, "- cumulative cashflow final:", cumulative_cashflow, "\n")
      }
    }

    cat("Simulation loops completed. Processing results...\n")

    # Calculate summary statistics
    cat("Calculating summary statistics...\n")
    customers_mean <- rowMeans(customers_matrix)
    cat("Customer means calculated\n")
    customers_q25 <- apply(customers_matrix, 1, quantile, probs = 0.25)
    cat("Customer Q25 calculated\n")
    customers_q75 <- apply(customers_matrix, 1, quantile, probs = 0.75)
    cat("Customer Q75 calculated\n")

    cashflow_mean <- rowMeans(cashflow_matrix)
    cat("Cashflow means calculated\n")
    cashflow_q25 <- apply(cashflow_matrix, 1, quantile, probs = 0.25)
    cat("Cashflow Q25 calculated\n")
    cashflow_q75 <- apply(cashflow_matrix, 1, quantile, probs = 0.75)
    cat("Cashflow Q75 calculated\n")

    # Create data frames
    cat("Creating data frames...\n")
    customers_df <- data.frame(
      Date = dates,
      Mean = customers_mean,
      Q25 = customers_q25,
      Q75 = customers_q75
    )
    cat("Customer data frame created with", nrow(customers_df), "rows\n")

    cashflow_df <- data.frame(
      Date = dates,
      Mean = cashflow_mean,
      Q25 = cashflow_q25,
      Q75 = cashflow_q75
    )
    cat("Cashflow data frame created with", nrow(cashflow_df), "rows\n")

    # Summary statistics
    cat("Calculating final summary statistics...\n")
    avg_daily_customers <- round(mean(customers_mean), 1)
    avg_daily_revenue <- round(mean(daily_expenses + (cashflow_mean[2:n_days] - cashflow_mean[1:(n_days - 1)])), 2)
    final_profit <- round(mean(cashflow_matrix[n_days, ]), 2)

    summary_stats <- list(
      avg_daily_customers = avg_daily_customers,
      avg_daily_revenue = avg_daily_revenue,
      final_profit = final_profit
    )

    cat("Summary stats: customers =", avg_daily_customers, ", revenue =", avg_daily_revenue, ", profit =", final_profit, "\n")

    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    cat("Total simulation time:", elapsed_time, "\n")
    cat("=== SIMULATION COMPLETE ===\n")

    return(list(
      customers = customers_df,
      cashflow = cashflow_df,
      summary = summary_stats,
      raw_customers = customers_matrix,
      raw_cashflow = cashflow_matrix
    ))
  }

  # Run simulation when button is clicked
  observeEvent(input$run_simulation, {
    cat("\n*** SIMULATION BUTTON CLICKED ***\n")
    cat("Input parameters:\n")
    cat("- max_customers:", input$max_customers, "\n")
    cat("- customers_mean:", input$customers_mean, "\n")
    cat("- customers_sd:", input$customers_sd, "\n")
    cat("- entry_fee:", input$entry_fee, "\n")
    cat("- consumables_mean:", input$consumables_mean, "\n")

    withProgress(message = "Running simulation...", value = 0, {
      incProgress(0.1, detail = "Initializing parameters")
      cat("Progress: Initializing parameters\n")

      # Validate inputs
      if (input$max_customers <= 0 || input$customers_mean <= 0) {
        cat("ERROR: Invalid input parameters\n")
        showNotification("Please check your input parameters.")
        return()
      }

      incProgress(0.3, detail = "Generating time series")
      cat("Progress: Generating time series\n")

      # Run the simulation
      cat("About to call run_business_simulation()...\n")
      results <- run_business_simulation()
      cat("run_business_simulation() completed!\n")

      incProgress(0.8, detail = "Processing results")
      cat("Progress: Processing results\n")

      # Store results
      cat("Storing results in reactive values...\n")
      simulation_data$customers_ts <- results$customers
      simulation_data$cashflow_ts <- results$cashflow
      simulation_data$summary_stats <- results$summary
      simulation_data$raw_data <- data.frame(
        Date = rep(results$customers$Date, 2),
        Type = rep(c("Customers", "Cashflow"), each = nrow(results$customers)),
        Mean = c(results$customers$Mean, results$cashflow$Mean),
        Q25 = c(results$customers$Q25, results$cashflow$Q25),
        Q75 = c(results$customers$Q75, results$cashflow$Q75)
      )
      cat("Results stored successfully!\n")

      incProgress(1.0, detail = "Complete!")
      cat("Progress: Complete!\n")

      showNotification("Simulation completed successfully!")
      cat("*** SIMULATION PROCESS FINISHED ***\n\n")
    })
  })

  # Customers plot
  output$customers_plot <- renderPlotly({
    if (is.null(simulation_data$customers_ts)) {
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

    df <- simulation_data$customers_ts

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
        title = "Customer Flow Over Time (100 Simulations)",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Customers per Day"),
        hovermode = "x unified"
      )

    p
  })

  # Cash flow plot
  output$cashflow_plot <- renderPlotly({
    if (is.null(simulation_data$cashflow_ts)) {
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
          yaxis = list(title = "Cumulative Cash Flow (€)"),
          title = "Cash Flow Over Time",
          showlegend = FALSE
        )
      return(p)
    }

    df <- simulation_data$cashflow_ts

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
        name = "Mean",
        mode = "lines"
      ) %>%
      layout(
        title = "Cumulative Cash Flow Over Time (100 Simulations)",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Cumulative Cash Flow (€)"),
        hovermode = "x unified"
      )

    p
  })

  # Summary value boxes
  output$avg_daily_customers <- renderValueBox({
    value <- if (is.null(simulation_data$summary_stats)) "Run Simulation" else simulation_data$summary_stats$avg_daily_customers
    valueBox(
      value = value,
      subtitle = "Avg Daily Customers",
      icon = icon("users"),
      color = "blue"
    )
  })

  output$avg_daily_revenue <- renderValueBox({
    value <- if (is.null(simulation_data$summary_stats)) "Run Simulation" else paste0("€", format(simulation_data$summary_stats$avg_daily_revenue, big.mark = ","))
    valueBox(
      value = value,
      subtitle = "Avg Daily Revenue",
      icon = icon("euro-sign"),
      color = "green"
    )
  })

  output$total_profit <- renderValueBox({
    value <- if (is.null(simulation_data$summary_stats)) "Run Simulation" else paste0("€", format(simulation_data$summary_stats$final_profit, big.mark = ","))
    box_color <- if (is.null(simulation_data$summary_stats)) {
      "yellow"
    } else if (simulation_data$summary_stats$final_profit >= 0) {
      "green"
    } else {
      "red"
    }
    valueBox(
      value = value,
      subtitle = "3-Year Profit",
      icon = icon("chart-line"),
      color = box_color
    )
  })

  # Results table
  output$results_table <- DT::renderDataTable({
    if (is.null(simulation_data$raw_data)) {
      return(data.frame(Message = "Run simulation to see detailed results"))
    }

    # Create monthly summary
    df <- simulation_data$customers_ts
    df$Month <- format(df$Date, "%Y-%m")
    monthly_summary <- df %>%
      group_by(.data$Month) %>%
      summarise(
        Avg_Customers = round(mean(.data$Mean), 1),
        Min_Customers = round(min(.data$Q25), 1),
        Max_Customers = round(max(.data$Q75), 1),
        .groups = "drop"
      )

    DT::datatable(monthly_summary,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) %>%
      DT::formatStyle(columns = 1:4, fontSize = "12px")
  })
}
