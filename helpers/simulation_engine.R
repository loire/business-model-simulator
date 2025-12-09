# simulation_engine.R - Modern simulation engine using data.table
# Refactored to use tabular data structures for parameters and results

library(data.table)
library(dplyr)
library(lubridate)

# Helper function for seasonal multipliers (copied from helper.R for independence)
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

# Main class for simulation parameters table
create_parameters_table <- function(params) {
    # Extract simulation setup
    n_days <- params$n_days
    start_date <- as.Date(params$start_date)
    dates <- seq(start_date, start_date + n_days - 1, by = "day")

    # Create base parameters table
    param_dt <- data.table(
        date = dates,
        day_index = 1:n_days,
        year = year(dates),
        month = month(dates),
        day_of_week = wday(dates), # Sunday = 1, Monday = 2
        day_name = weekdays(dates),

        # Customer parameters
        max_customers = params$max_customers,
        base_customers = params$base_customers,
        customer_sd = params$customers_sd,

        # Multipliers (will be calculated)
        weekly_multiplier = 1.0,
        seasonal_multiplier = 1.0,
        trend_multiplier = 1.0,
        combined_multiplier = 1.0,

        # Expected values
        expected_customers = 0.0,

        # Revenue parameters (daily totals)
        revenue_entry_fee = 0.0,
        revenue_consumables = 0.0,
        revenue_other = 0.0,
        total_expected_revenue = 0.0,

        # Expense parameters (daily amounts)
        expenses_daily = 0.0,
        expenses_weekly = 0.0,
        expenses_monthly = 0.0,
        expenses_yearly = 0.0,
        expenses_investment = 0.0,
        total_expected_expenses = 0.0,

        # Net expectations
        net_daily_cashflow = 0.0
    )

    return(param_dt)
}

# Calculate all multipliers for the parameters table
update_parameters_multipliers <- function(param_dt, weekly_multipliers, seasonal_periods, trend_params) {
    # Weekly multipliers - convert Sunday=1 to Monday=1 indexing
    param_dt[, weekly_multiplier := weekly_multipliers[ifelse(day_of_week == 1, 7, day_of_week - 1)]]

    # Seasonal multipliers
    param_dt[, seasonal_multiplier := sapply(date, function(d) {
        calculate_seasonal_multiplier(d, seasonal_periods)
    })]

    # Trend multipliers (growth/decline over time)
    if (trend_params$enabled) {
        days_elapsed <- param_dt$day_index - 1
        annual_rate <- trend_params$annual_rate
        param_dt[, trend_multiplier := (1 + annual_rate)^(days_elapsed / 365)]
    } else {
        param_dt[, trend_multiplier := 1.0]
    }

    # Combined multiplier
    param_dt[, combined_multiplier := weekly_multiplier * seasonal_multiplier * trend_multiplier]

    # Expected customers (deterministic for parameters table)
    param_dt[, expected_customers := pmax(0, pmin(
        max_customers,
        base_customers * combined_multiplier
    ))]

    return(param_dt)
}

# Calculate revenue expectations for parameters table
update_parameters_revenue <- function(param_dt, revenue_items) {
    # Reset revenue columns
    param_dt[, `:=`(
        revenue_entry_fee = 0.0,
        revenue_consumables = 0.0,
        revenue_other = 0.0
    )]

    # Calculate revenue for each item type
    for (item_name in names(revenue_items)) {
        item <- revenue_items[[item_name]]

        if (item$type == "fixed_per_customer") {
            # Entry fees
            param_dt[, revenue_entry_fee := revenue_entry_fee + (expected_customers * item$price)]
        } else if (item$type == "poisson_per_customer") {
            # Consumables with Poisson distribution
            expected_purchases <- param_dt$expected_customers * item$lambda
            param_dt[, revenue_consumables := revenue_consumables + (expected_purchases * item$price)]
        } else if (item$type == "probability_per_customer") {
            # Other items with probability
            expected_purchases <- param_dt$expected_customers * item$probability * item$lambda
            param_dt[, revenue_other := revenue_other + (expected_purchases * item$price)]
        }
    }

    # Total expected revenue
    param_dt[, total_expected_revenue := revenue_entry_fee + revenue_consumables + revenue_other]

    return(param_dt)
}

# Calculate expense expectations for parameters table
update_parameters_expenses <- function(param_dt, expenses, investment_dates = list()) {
    # Reset expense columns
    param_dt[, `:=`(
        expenses_daily = 0.0,
        expenses_weekly = 0.0,
        expenses_monthly = 0.0,
        expenses_yearly = 0.0,
        expenses_investment = 0.0
    )]

    # Process each expense
    for (exp_name in names(expenses)) {
        expense <- expenses[[exp_name]]

        if (expense$freq == "daily") {
            param_dt[, expenses_daily := expenses_daily + expense$value]
        } else if (expense$freq == "weekly") {
            # Weekly expenses on Mondays
            param_dt[day_of_week == 1, expenses_weekly := expenses_weekly + expense$value]
        } else if (expense$freq == "monthly") {
            # Monthly expenses on 1st of month
            param_dt[mday(date) == 1, expenses_monthly := expenses_monthly + expense$value]
        } else if (expense$freq == "yearly") {
            # Yearly expenses on January 1st
            param_dt[month == 1 & mday(date) == 1, expenses_yearly := expenses_yearly + expense$value]
        } else if (expense$freq == "investment") {
            # One-time investments on specified dates
            if (exp_name %in% names(investment_dates)) {
                investment_date <- investment_dates[[exp_name]]
                param_dt[date == investment_date, expenses_investment := expenses_investment + expense$value]
            }
        }
    }

    # Total expected expenses
    param_dt[, total_expected_expenses := expenses_daily + expenses_weekly + expenses_monthly + expenses_yearly + expenses_investment]

    # Net daily cashflow expectation
    param_dt[, net_daily_cashflow := total_expected_revenue - total_expected_expenses]

    return(param_dt)
}

# Main function to create complete parameters table
create_complete_parameters_table <- function(params) {
    # Create base table
    param_dt <- create_parameters_table(params)

    # Update all calculations
    param_dt <- update_parameters_multipliers(param_dt, params$weekly_multipliers, params$seasonal_periods, params$trend_params)
    param_dt <- update_parameters_revenue(param_dt, params$revenue_items)
    param_dt <- update_parameters_expenses(param_dt, params$expenses, params$investment_dates)

    return(param_dt)
}

# Create simulation results table structure
create_results_table <- function(param_dt, n_iterations) {
    n_days <- nrow(param_dt)

    # Create results table with one row per day per iteration
    results_dt <- data.table(
        iteration = rep(1:n_iterations, each = n_days),
        day_index = rep(1:n_days, n_iterations),
        date = rep(param_dt$date, n_iterations),

        # Simulated values
        actual_customers = 0L,
        actual_revenue_entry = 0.0,
        actual_revenue_consumables = 0.0,
        actual_revenue_other = 0.0,
        actual_total_revenue = 0.0,
        actual_expenses_daily = 0.0,
        actual_expenses_periodic = 0.0,
        actual_total_expenses = 0.0,
        daily_cashflow = 0.0,
        cumulative_cashflow = 0.0
    )

    return(results_dt)
}

# Run Monte Carlo simulation using the new structure
run_modern_simulation <- function(param_dt, revenue_items, expenses, n_iterations = 100) {
    cat("=== MODERN SIMULATION START ===\n")
    start_time <- Sys.time()

    n_days <- nrow(param_dt)

    # Create results table
    results_dt <- create_results_table(param_dt, n_iterations)

    cat("Running", n_iterations, "iterations for", n_days, "days\n")

    # Vectorized simulation by iteration
    for (iter in 1:n_iterations) {
        if (iter %% 20 == 0 || iter <= 5) {
            cat("Iteration", iter, "of", n_iterations, "\n")
        }

        # Get rows for this iteration
        iter_rows <- results_dt[iteration == iter]
        iter_indices <- which(results_dt$iteration == iter)

        # Simulate customers using expected values from param_dt
        simulated_customers <- pmax(0, pmin(
            param_dt$max_customers,
            rnorm(n_days, param_dt$expected_customers, param_dt$customer_sd)
        ))
        simulated_customers <- round(simulated_customers)

        # Simulate revenues
        revenue_results <- simulate_daily_revenues(simulated_customers, revenue_items)

        # Simulate expenses
        expense_results <- simulate_daily_expenses(param_dt, expenses)

        # Update results table
        results_dt[iter_indices, `:=`(
            actual_customers = simulated_customers,
            actual_revenue_entry = revenue_results$entry_revenue,
            actual_revenue_consumables = revenue_results$consumable_revenue,
            actual_revenue_other = revenue_results$other_revenue,
            actual_total_revenue = revenue_results$total_revenue,
            actual_expenses_daily = expense_results$daily_expenses,
            actual_expenses_periodic = expense_results$periodic_expenses,
            actual_total_expenses = expense_results$total_expenses
        )]

        # Calculate cashflows
        results_dt[iter_indices, daily_cashflow := actual_total_revenue - actual_total_expenses]
        results_dt[iter_indices, cumulative_cashflow := cumsum(daily_cashflow)]
    }

    end_time <- Sys.time()
    cat("Simulation completed in", round(difftime(end_time, start_time, units = "secs"), 2), "seconds\n")

    return(results_dt)
}

# Simulate daily revenues using vectorized operations
simulate_daily_revenues <- function(customers_vector, revenue_items) {
    n_days <- length(customers_vector)

    entry_revenue <- numeric(n_days)
    consumable_revenue <- numeric(n_days)
    other_revenue <- numeric(n_days)

    for (item_name in names(revenue_items)) {
        item <- revenue_items[[item_name]]

        # Simulate prices with variability
        daily_prices <- rnorm(n_days, item$price, item$price_sd)
        daily_prices <- pmax(0, daily_prices) # No negative prices

        if (item$type == "fixed_per_customer") {
            entry_revenue <- entry_revenue + (customers_vector * daily_prices)
        } else if (item$type == "poisson_per_customer") {
            # Poisson purchases per customer
            total_purchases <- sapply(customers_vector, function(n_cust) {
                if (n_cust == 0) {
                    return(0)
                }
                sum(rpois(n_cust, item$lambda))
            })
            consumable_revenue <- consumable_revenue + (total_purchases * daily_prices)
        } else if (item$type == "probability_per_customer") {
            # Probability-based purchases
            total_purchases <- sapply(customers_vector, function(n_cust) {
                if (n_cust == 0) {
                    return(0)
                }
                sum(rbinom(n_cust, 1, item$probability) * rpois(n_cust, item$lambda))
            })
            other_revenue <- other_revenue + (total_purchases * daily_prices)
        }
    }

    return(list(
        entry_revenue = entry_revenue,
        consumable_revenue = consumable_revenue,
        other_revenue = other_revenue,
        total_revenue = entry_revenue + consumable_revenue + other_revenue
    ))
}

# Simulate daily expenses using vectorized operations
simulate_daily_expenses <- function(param_dt, expenses) {
    n_days <- nrow(param_dt)

    daily_expenses <- numeric(n_days)
    periodic_expenses <- numeric(n_days)

    for (exp_name in names(expenses)) {
        expense <- expenses[[exp_name]]

        # Simulate expense amounts with variability
        daily_amounts <- rnorm(n_days, expense$value, expense$sd)
        daily_amounts <- pmax(0, daily_amounts) # No negative expenses

        if (expense$freq == "daily") {
            daily_expenses <- daily_expenses + daily_amounts
        } else if (expense$freq %in% c("weekly", "monthly", "yearly", "investment")) {
            # Use the pre-calculated periodic expenses from param_dt
            periodic_mask <- (param_dt$expenses_weekly > 0 | param_dt$expenses_monthly > 0 |
                param_dt$expenses_yearly > 0 | param_dt$expenses_investment > 0)

            if (expense$freq == "weekly") {
                weekly_mask <- param_dt$expenses_weekly > 0
                periodic_expenses[weekly_mask] <- periodic_expenses[weekly_mask] + daily_amounts[weekly_mask]
            } else if (expense$freq == "monthly") {
                monthly_mask <- param_dt$expenses_monthly > 0
                periodic_expenses[monthly_mask] <- periodic_expenses[monthly_mask] + daily_amounts[monthly_mask]
            } else if (expense$freq == "yearly") {
                yearly_mask <- param_dt$expenses_yearly > 0
                periodic_expenses[yearly_mask] <- periodic_expenses[yearly_mask] + daily_amounts[yearly_mask]
            } else if (expense$freq == "investment") {
                investment_mask <- param_dt$expenses_investment > 0
                periodic_expenses[investment_mask] <- periodic_expenses[investment_mask] + daily_amounts[investment_mask]
            }
        }
    }

    return(list(
        daily_expenses = daily_expenses,
        periodic_expenses = periodic_expenses,
        total_expenses = daily_expenses + periodic_expenses
    ))
}

# Aggregate simulation results for visualization
aggregate_simulation_results <- function(results_dt) {
    # Daily aggregations across iterations
    daily_summary <- results_dt[, .(
        mean_customers = mean(actual_customers),
        q25_customers = quantile(actual_customers, 0.25),
        q75_customers = quantile(actual_customers, 0.75),
        min_customers = min(actual_customers),
        max_customers = max(actual_customers),
        mean_revenue = mean(actual_total_revenue),
        q25_revenue = quantile(actual_total_revenue, 0.25),
        q75_revenue = quantile(actual_total_revenue, 0.75),
        mean_expenses = mean(actual_total_expenses),
        q25_expenses = quantile(actual_total_expenses, 0.25),
        q75_expenses = quantile(actual_total_expenses, 0.75),
        mean_cashflow = mean(cumulative_cashflow),
        q25_cashflow = quantile(cumulative_cashflow, 0.25),
        q75_cashflow = quantile(cumulative_cashflow, 0.75),
        min_cashflow = min(cumulative_cashflow),
        max_cashflow = max(cumulative_cashflow)
    ), by = .(date)]

    # Summary statistics
    final_results <- results_dt[day_index == max(day_index)]

    summary_stats <- list(
        avg_daily_customers = mean(daily_summary$mean_customers),
        avg_daily_revenue = mean(daily_summary$mean_revenue),
        final_profit = mean(final_results$cumulative_cashflow),
        profit_probability = mean(final_results$cumulative_cashflow > 0),
        breakeven_days = NA
    )

    # Find breakeven point
    positive_cashflow <- daily_summary[mean_cashflow > 0]
    if (nrow(positive_cashflow) > 0) {
        breakeven_date <- min(positive_cashflow$date)
        summary_stats$breakeven_days <- as.numeric(breakeven_date - min(daily_summary$date))
    }

    return(list(
        daily_summary = daily_summary,
        summary_stats = summary_stats,
        raw_results = results_dt
    ))
}

# Save parameters table to file
save_parameters_table <- function(param_dt, filename = "simulation_parameters.csv") {
    fwrite(param_dt, filename)
    cat("Parameters saved to", filename, "\n")
}

# Save results table to file
save_results_table <- function(results_dt, filename = "simulation_results.csv") {
    fwrite(results_dt, filename)
    cat("Results saved to", filename, "\n")
}

# Load parameters table from file
load_parameters_table <- function(filename = "simulation_parameters.csv") {
    if (file.exists(filename)) {
        param_dt <- fread(filename)
        param_dt[, date := as.Date(date)]
        cat("Parameters loaded from", filename, "\n")
        return(param_dt)
    } else {
        stop("File not found:", filename)
    }
}

# Load results table from file
load_results_table <- function(filename = "simulation_results.csv") {
    if (file.exists(filename)) {
        results_dt <- fread(filename)
        results_dt[, date := as.Date(date)]
        cat("Results loaded from", filename, "\n")
        return(results_dt)
    } else {
        stop("File not found:", filename)
    }
}

# Export data for plotting (converts to standard data.frame format)
prepare_plot_data <- function(daily_summary, data_type = "customers") {
    df <- as.data.frame(daily_summary)

    if (data_type == "customers") {
        df_plot <- data.frame(
            Date = df$date,
            Mean = df$mean_customers,
            Q25 = df$q25_customers,
            Q75 = df$q75_customers,
            Min = df$min_customers,
            Max = df$max_customers
        )
    } else if (data_type == "cashflow") {
        df_plot <- data.frame(
            Date = df$date,
            Mean = df$mean_cashflow,
            Q25 = df$q25_cashflow,
            Q75 = df$q75_cashflow,
            Min = df$min_cashflow,
            Max = df$max_cashflow
        )
    } else if (data_type == "revenue") {
        df_plot <- data.frame(
            Date = df$date,
            Mean = df$mean_revenue,
            Q25 = df$q25_revenue,
            Q75 = df$q75_revenue
        )
    }

    return(df_plot)
}

# Aggregate simulation results by month for monthly profit/loss visualization
aggregate_monthly_results <- function(results_dt) {
    # Add year-month column
    results_dt[, year_month := format(date, "%Y-%m")]
    results_dt[, month_start := as.Date(paste0(year_month, "-01"))]

    # Calculate monthly aggregations per iteration
    monthly_by_iteration <- results_dt[, .(
        monthly_revenue = sum(actual_total_revenue, na.rm = TRUE),
        monthly_expenses = sum(actual_total_expenses, na.rm = TRUE),
        monthly_profit = sum(daily_cashflow, na.rm = TRUE)
    ), by = .(iteration, year_month, month_start)]

    # Calculate statistics across iterations for each month
    monthly_summary <- monthly_by_iteration[, .(
        mean_revenue = mean(monthly_revenue, na.rm = TRUE),
        q25_revenue = quantile(monthly_revenue, 0.25, na.rm = TRUE),
        q75_revenue = quantile(monthly_revenue, 0.75, na.rm = TRUE),
        min_revenue = min(monthly_revenue, na.rm = TRUE),
        max_revenue = max(monthly_revenue, na.rm = TRUE),
        mean_expenses = mean(monthly_expenses, na.rm = TRUE),
        q25_expenses = quantile(monthly_expenses, 0.25, na.rm = TRUE),
        q75_expenses = quantile(monthly_expenses, 0.75, na.rm = TRUE),
        min_expenses = min(monthly_expenses, na.rm = TRUE),
        max_expenses = max(monthly_expenses, na.rm = TRUE),
        mean_profit = mean(monthly_profit, na.rm = TRUE),
        q25_profit = quantile(monthly_profit, 0.25, na.rm = TRUE),
        q75_profit = quantile(monthly_profit, 0.75, na.rm = TRUE),
        min_profit = min(monthly_profit, na.rm = TRUE),
        max_profit = max(monthly_profit, na.rm = TRUE)
    ), by = .(year_month, month_start)]

    # Order by month
    monthly_summary <- monthly_summary[order(month_start)]

    return(monthly_summary)
}
