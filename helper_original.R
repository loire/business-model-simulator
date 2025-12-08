# helper.R - Helper functions for Business Model Simulation
# Contains all computational functions used during simulations

# UI Helper functions
create_help_icon <- function(help_key) {
    help_text <- HELP_TEXT[[help_key]]
    if (is.null(help_text)) {
        return(NULL)
    }

    tags$span(
        class = "help-icon",
        style = "margin-left: 5px; color: #337ab7; cursor: help;",
        icon("question-circle"),
        title = help_text,
        `data-toggle` = "tooltip",
        `data-placement` = "top"
    )
}

create_slider_with_input <- function(inputId, label, min, max, value, step = 1, help_key = NULL, currency_symbol = NULL) {
    label_with_help <- if (!is.null(help_key)) {
        tagList(label, create_help_icon(help_key))
    } else {
        label
    }

    # Add currency symbol to label if provided
    if (!is.null(currency_symbol)) {
        label_with_help <- tagList(label_with_help, paste0(" (", currency_symbol, ")"))
    }

    div(
        style = "margin-bottom: 15px;",
        tags$label(label_with_help, `for` = inputId),
        fluidRow(
            column(
                8,
                sliderInput(inputId, NULL,
                    min = min, max = max, value = value, step = step
                )
            ),
            column(
                4,
                numericInput(paste0(inputId, "_num"), NULL,
                    value = value, min = min, max = max, step = step
                )
            )
        )
    )
}

# Currency conversion functions
convert_currency <- function(amount, from_currency, to_currency) {
    if (from_currency == to_currency) {
        return(amount)
    }

    # Handle VND scaling in conversion
    actual_amount <- amount
    if (is_vnd_currency(from_currency)) {
        # If converting FROM VND display, convert to actual VND first
        actual_amount <- format_vnd_actual(amount)
    }

    from_rate <- CURRENCIES[[from_currency]]$rate
    to_rate <- CURRENCIES[[to_currency]]$rate

    # Convert to base currency (EUR) then to target currency
    base_amount <- actual_amount / from_rate
    converted_amount <- base_amount * to_rate

    # Handle VND scaling for output
    if (is_vnd_currency(to_currency)) {
        # If converting TO VND, return display value (in thousands)
        return(format_vnd_display(converted_amount))
    }

    return(converted_amount)
}

get_currency_symbol <- function(currency) {
    return(CURRENCIES[[currency]]$symbol)
}

get_currency_name <- function(currency) {
    return(CURRENCIES[[currency]]$name)
}

# VND scaling functions - display in thousands for readability
is_vnd_currency <- function(currency) {
    return(currency == "VND")
}

format_vnd_display <- function(value) {
    # Convert VND to thousands for display (100K VND = 100,000 VND)
    return(value / 1000)
}

format_vnd_actual <- function(display_value) {
    # Convert display value back to actual VND
    return(display_value * 1000)
}

get_currency_display_symbol <- function(currency) {
    if (is_vnd_currency(currency)) {
        return("K-₫") # Display as thousands of VND
    }
    return(CURRENCIES[[currency]]$symbol)
}

# Function to get appropriate slider ranges based on currency
get_currency_adjusted_ranges <- function(currency) {
    base_ranges <- SLIDER_RANGES

    if (is_vnd_currency(currency)) {
        # For VND, we need much larger ranges but display in thousands
        # Convert base EUR ranges to VND and then to display units (thousands)
        vnd_rate <- CURRENCIES[["VND"]]$rate

        # Adjust monetary ranges for VND (in thousands)
        base_ranges$price$max <- format_vnd_display(base_ranges$price$max * vnd_rate)
        base_ranges$price$step <- format_vnd_display(base_ranges$price$step * vnd_rate)

        base_ranges$price_sd$max <- format_vnd_display(base_ranges$price_sd$max * vnd_rate)
        base_ranges$price_sd$step <- format_vnd_display(base_ranges$price_sd$step * vnd_rate)

        base_ranges$expense_value$max <- format_vnd_display(base_ranges$expense_value$max * vnd_rate)
        base_ranges$expense_value$step <- format_vnd_display(base_ranges$expense_value$step * vnd_rate)

        base_ranges$expense_sd$max <- format_vnd_display(base_ranges$expense_sd$max * vnd_rate)
        base_ranges$expense_sd$step <- format_vnd_display(base_ranges$expense_sd$step * vnd_rate)
    } else if (currency == "USD") {
        # For USD, slightly adjust ranges
        usd_rate <- CURRENCIES[["USD"]]$rate

        base_ranges$price$max <- base_ranges$price$max * usd_rate
        base_ranges$price_sd$max <- base_ranges$price_sd$max * usd_rate
        base_ranges$expense_value$max <- base_ranges$expense_value$max * usd_rate
        base_ranges$expense_sd$max <- base_ranges$expense_sd$max * usd_rate
    }

    return(base_ranges)
}

# Function to convert all monetary values in a list to new currency
convert_monetary_list <- function(monetary_list, from_currency, to_currency) {
    if (from_currency == to_currency) {
        return(monetary_list)
    }

    converted_list <- monetary_list
    monetary_fields <- c("value", "sd", "price", "price_sd")

    for (item_name in names(converted_list)) {
        for (field in monetary_fields) {
            if (field %in% names(converted_list[[item_name]])) {
                # First convert from actual values if coming from VND
                source_value <- converted_list[[item_name]][[field]]
                if (is_vnd_currency(from_currency)) {
                    # Convert FROM display to actual VND for proper conversion
                    source_value <- format_vnd_actual(source_value)
                }

                # Perform currency conversion using actual values
                from_rate <- CURRENCIES[[from_currency]]$rate
                to_rate <- CURRENCIES[[to_currency]]$rate
                base_amount <- source_value / from_rate
                converted_amount <- base_amount * to_rate

                # Convert TO display format if targeting VND
                if (is_vnd_currency(to_currency)) {
                    converted_amount <- format_vnd_display(converted_amount)
                }

                converted_list[[item_name]][[field]] <- converted_amount
            }
        }
    }

    return(converted_list)
}

# Function to convert frequency to daily multiplier
freq_to_daily <- function(frequency) {
    switch(frequency,
        "daily" = 1,
        "weekly" = 1 / 7,
        "monthly" = 1 / 30.44, # Average days per month
        "yearly" = 1 / 365.25 # Average days per year
    )
}

# Function to calculate expenses for a specific day (periodic scheduling)
calculate_expenses_for_day <- function(day, current_date, start_date, expense_list) {
    total_expenses <- 0

    for (expense_name in names(expense_list)) {
        expense <- expense_list[[expense_name]]

        # Skip investment expenses (handled separately)
        if (expense$freq == "investment") next

        # Check if expense is due on this day
        if (is_expense_due(day, current_date, start_date, expense$freq)) {
            # Add random variation if specified
            expense_amount <- expense$value
            if (expense$sd > 0) {
                expense_amount <- rnorm(1, mean = expense$value, sd = expense$sd)
                expense_amount <- max(0, expense_amount) # Ensure non-negative
            }
            total_expenses <- total_expenses + expense_amount
        }
    }

    return(total_expenses)
}

# Function to determine if an expense is due on a specific day
is_expense_due <- function(day, current_date, start_date, frequency) {
    switch(frequency,
        "daily" = TRUE, # Due every day
        "weekly" = {
            # Due every 7 days starting from day 1
            (day - 1) %% 7 == 0
        },
        "monthly" = {
            # Due on the same day of month as start date
            format(current_date, "%d") == format(start_date, "%d")
        },
        "yearly" = {
            # Due on the same month-day as start date
            format(current_date, "%m-%d") == format(start_date, "%m-%d")
        },
        FALSE # Unknown frequency
    )
}

# Legacy function for backwards compatibility (now deprecated)
calculate_daily_expenses <- function(expense_list) {
    warning("calculate_daily_expenses is deprecated. Use calculate_expenses_for_day instead.")
    daily_expenses <- 0

    for (expense_name in names(expense_list)) {
        expense <- expense_list[[expense_name]]
        if (expense$freq != "investment") {
            daily_multiplier <- freq_to_daily(expense$freq)
            daily_expense <- expense$value * daily_multiplier
            daily_expenses <- daily_expenses + daily_expense
        }
    }

    return(daily_expenses)
}

# Function to calculate investment for a specific day
calculate_investment_for_day <- function(current_date, start_date, investment_dates, expense_list) {
    total_investment <- 0

    for (expense_name in names(expense_list)) {
        expense <- expense_list[[expense_name]]
        if (expense$freq == "investment") {
            # Check if this investment should occur on this date
            investment_date <- investment_dates[[expense_name]]
            if (!is.null(investment_date) && as.Date(current_date) == as.Date(investment_date)) {
                # Add random variation if specified
                investment_amount <- expense$value
                if (expense$sd > 0) {
                    investment_amount <- rnorm(1, mean = expense$value, sd = expense$sd)
                    investment_amount <- max(0, investment_amount) # Ensure non-negative
                }
                total_investment <- total_investment + investment_amount
            }
        }
    }

    return(total_investment)
}

# Legacy function for backwards compatibility
calculate_investment <- function(expense_list) {
    warning("calculate_investment is deprecated. Use calculate_investment_for_day instead.")
    total_investment <- 0

    for (expense_name in names(expense_list)) {
        expense <- expense_list[[expense_name]]
        if (expense$freq == "investment") {
            total_investment <- total_investment + expense$value
        }
    }

    return(total_investment)
}

# Function to get day of week (1=Monday, 7=Sunday)
get_day_of_week <- function(date) {
    as.numeric(format(date, "%u"))
}

# Function to calculate seasonal multiplier for a specific date
calculate_seasonal_multiplier <- function(date, seasonal_periods) {
    multiplier <- 1.0

    # Apply seasonal adjustments (holidays, special periods)
    for (period in seasonal_periods) {
        is_match <- FALSE

        if (period$recurring == "yearly") {
            # For yearly recurring periods, check month-day only
            current_md <- format(date, "%m-%d")
            start_md <- period$start
            end_md <- period$end

            # Handle year-spanning periods (e.g., Dec 20 - Jan 5)
            if (start_md <= end_md) {
                is_match <- (current_md >= start_md && current_md <= end_md)
            } else {
                is_match <- (current_md >= start_md || current_md <= end_md)
            }
        } else {
            # One-time periods with full dates
            period_start <- as.Date(period$start)
            period_end <- as.Date(period$end)
            is_match <- (date >= period_start && date <= period_end)
        }

        if (is_match) {
            multiplier <- multiplier * period$multiplier
            break # Apply only the first matching period
        }
    }

    return(multiplier)
}

# Function to calculate expected customers for a specific day
calculate_expected_customers <- function(date, base_customers, weekly_multipliers,
                                         seasonal_periods, trend_params) {
    # Base customer count
    expected <- base_customers

    # Apply day-of-week multiplier
    day_of_week <- get_day_of_week(date)
    expected <- expected * weekly_multipliers[day_of_week]

    # Apply seasonal multiplier
    seasonal_multiplier <- calculate_seasonal_multiplier(date, seasonal_periods)
    expected <- expected * seasonal_multiplier

    # Apply time trend (growth/decline over time)
    if (!is.null(trend_params) && trend_params$enabled) {
        start_date <- as.Date(trend_params$start_date)
        days_elapsed <- as.numeric(date - start_date)

        # Linear trend: multiplier = 1 + (trend_rate * days_elapsed / 365)
        trend_multiplier <- 1 + (trend_params$annual_rate * days_elapsed / 365.25)
        expected <- expected * max(0.1, trend_multiplier) # Minimum 10% of original
    }

    return(expected)
}

# Function to generate daily customer count
generate_daily_customers <- function(expected_customers, sd_customers, max_capacity) {
    customers_raw <- rnorm(1, mean = expected_customers, sd = sd_customers)
    customers <- max(0, min(round(customers_raw), max_capacity))
    return(customers)
}

# Function to calculate revenue from custom revenue items
calculate_custom_revenue <- function(customers, revenue_items) {
    total_revenue <- 0

    for (item_name in names(revenue_items)) {
        item <- revenue_items[[item_name]]

        if (item$type == "fixed_per_customer") {
            # Fixed amount per customer
            item_revenue <- customers * rnorm(1, mean = item$price, sd = item$price_sd)
        } else if (item$type == "poisson_per_customer") {
            # Poisson-distributed quantity per customer
            if (customers > 0) {
                quantities <- rpois(customers, lambda = item$lambda)
                prices <- rnorm(customers, mean = item$price, sd = item$price_sd)
                item_revenue <- sum(quantities * pmax(0, prices)) # Ensure no negative prices
            } else {
                item_revenue <- 0
            }
        } else if (item$type == "probability_per_customer") {
            # Each customer has probability of purchasing
            if (customers > 0) {
                purchases <- rbinom(customers, 1, prob = item$probability)
                quantities <- rpois(sum(purchases), lambda = item$lambda)
                prices <- rnorm(length(quantities), mean = item$price, sd = item$price_sd)
                item_revenue <- sum(quantities * pmax(0, prices))
            } else {
                item_revenue <- 0
            }
        } else {
            item_revenue <- 0
        }

        total_revenue <- total_revenue + max(0, item_revenue)
    }

    return(total_revenue)
}

# Function to run the complete business simulation
run_complete_simulation <- function(params) {
    cat("=== SIMULATION START ===\n")
    start_time <- Sys.time()

    # Extract parameters
    n_iterations <- params$n_iterations
    n_days <- params$n_days
    start_date <- as.Date(params$start_date)
    dates <- seq(start_date, start_date + n_days - 1, by = "day")

    cat("Simulation parameters:\n")
    cat("- Iterations:", n_iterations, "\n")
    cat("- Days:", n_days, "\n")
    cat("- Date range:", as.character(start_date), "to", as.character(start_date + n_days - 1), "\n")

    # Initialize result matrices
    cat("Initializing matrices...\n")
    customers_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)
    cashflow_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)

    # Show investment dates information
    cat("Calculating expenses with periodic scheduling...\n")
    if (length(params$investment_dates) > 0) {
        cat("Investment dates scheduled:\n")
        for (exp_name in names(params$investment_dates)) {
            cat("  -", exp_name, ":", as.character(params$investment_dates[[exp_name]]), "\n")
        }
    } else {
        cat("No investment expenses configured.\n")
    }

    # Run simulations
    cat("Starting simulation loops...\n")
    for (iter in 1:n_iterations) {
        if (iter %% 10 == 0 || iter <= 5) {
            cat("Iteration", iter, "of", n_iterations, "\n")
        }

        cumulative_cashflow <- 0 # Start with zero, investment will be applied on scheduled day

        for (day in 1:n_days) {
            current_date <- dates[day]

            # Calculate expected customers for this day
            expected_customers <- calculate_expected_customers(
                current_date,
                params$base_customers,
                params$weekly_multipliers,
                params$seasonal_periods,
                params$trend_params
            )

            # Generate actual customer count
            customers <- generate_daily_customers(
                expected_customers,
                params$customers_sd,
                params$max_customers
            )
            customers_matrix[day, iter] <- customers

            # Calculate daily revenue
            daily_revenue <- calculate_custom_revenue(customers, params$revenue_items)

            # Calculate expenses for this day (periodic scheduling)
            daily_expenses <- calculate_expenses_for_day(day, current_date, start_date, params$expenses)

            # Calculate investment for this day
            daily_investment <- calculate_investment_for_day(current_date, start_date, params$investment_dates, params$expenses)

            # Daily profit/loss
            daily_profit <- daily_revenue - daily_expenses - daily_investment
            cumulative_cashflow <- cumulative_cashflow + daily_profit
            cashflow_matrix[day, iter] <- cumulative_cashflow

            # Logging for first iteration
            if (iter == 1 && (day <= 3 || day %% 100 == 0)) {
                expense_note <- if (daily_expenses > 0) paste("expenses =", round(daily_expenses, 2)) else ""
                investment_note <- if (daily_investment > 0) paste("investment =", round(daily_investment, 2)) else ""
                additional_info <- paste(c(expense_note, investment_note), collapse = " ")

                cat(
                    "  Day", day, "(", as.character(current_date), "):",
                    "expected =", round(expected_customers, 1),
                    "actual =", customers,
                    "revenue =", round(daily_revenue, 2),
                    additional_info,
                    "cumulative =", round(cumulative_cashflow, 2), "\n"
                )
            }
        }

        if (iter %% 25 == 0) {
            cat("Completed iteration", iter, "- final cashflow:", round(cumulative_cashflow, 2), "\n")
        }
    }

    cat("Simulation loops completed. Processing results...\n")

    # Calculate summary statistics
    customers_mean <- rowMeans(customers_matrix)
    customers_q25 <- apply(customers_matrix, 1, quantile, probs = 0.25)
    customers_q75 <- apply(customers_matrix, 1, quantile, probs = 0.75)

    cashflow_mean <- rowMeans(cashflow_matrix)
    cashflow_q25 <- apply(cashflow_matrix, 1, quantile, probs = 0.25)
    cashflow_q75 <- apply(cashflow_matrix, 1, quantile, probs = 0.75)

    # Create data frames
    customers_df <- data.frame(
        Date = dates,
        Mean = customers_mean,
        Q25 = customers_q25,
        Q75 = customers_q75
    )

    cashflow_df <- data.frame(
        Date = dates,
        Mean = cashflow_mean,
        Q25 = cashflow_q25,
        Q75 = cashflow_q75
    )

    # Summary statistics
    avg_daily_customers <- round(mean(customers_mean), 1)
    avg_daily_revenue <- round(mean(daily_expenses + (cashflow_mean[2:n_days] - cashflow_mean[1:(n_days - 1)])), 2)
    final_profit <- round(mean(cashflow_matrix[n_days, ]), 2)

    summary_stats <- list(
        avg_daily_customers = avg_daily_customers,
        avg_daily_revenue = avg_daily_revenue,
        final_profit = final_profit
    )

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

# Helper function to validate simulation parameters
validate_simulation_params <- function(params) {
    errors <- c()

    if (params$max_customers <= 0) {
        errors <- c(errors, "Maximum customers must be greater than 0")
    }

    if (params$base_customers <= 0) {
        errors <- c(errors, "Base customers must be greater than 0")
    }

    if (params$n_iterations <= 0) {
        errors <- c(errors, "Number of iterations must be greater than 0")
    }

    if (params$n_days <= 0) {
        errors <- c(errors, "Number of days must be greater than 0")
    }

    return(errors)
}
