# Test script to debug simulation issues
# This script simulates the simulation function independently

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

# Mock input values
input <- list(
    max_customers = 100,
    customers_mean = 50,
    customers_sd = 15,
    entry_fee = 15,
    entry_fee_sd = 2,
    consumables_mean = 1,
    consumables_price = 3,
    consumables_price_sd = 0.5,
    rent_value = 2000,
    rent_sd = 200,
    rent_freq = "monthly",
    taxes_value = 5000,
    taxes_sd = 500,
    taxes_freq = "yearly",
    consumables_cost_value = 300,
    consumables_cost_sd = 50,
    consumables_cost_freq = "weekly",
    it_value = 2000,
    it_sd = 300,
    it_freq = "yearly",
    material_value = 15000,
    material_sd = 2000,
    material_freq = "investment",
    staff_value = 4000,
    staff_sd = 400,
    staff_freq = "monthly"
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

# Test the simulation
cat("=== TESTING SIMULATION INDEPENDENTLY ===\n")

# Test expense calculations
cat("Testing expense calculations...\n")
daily_exp <- calculate_daily_expenses()
investment <- calculate_investment()
cat("Daily expenses:", daily_exp, "\n")
cat("Investment:", investment, "\n")

# Test small simulation
cat("Running mini simulation (5 iterations, 10 days)...\n")
n_iterations <- 5
n_days <- 10

customers_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)
cashflow_matrix <- matrix(0, nrow = n_days, ncol = n_iterations)

for (iter in 1:n_iterations) {
    cat("Iteration", iter, "\n")
    cumulative_cashflow <- -investment

    for (day in 1:n_days) {
        # Generate customers
        customers_raw <- rnorm(1, mean = input$customers_mean, sd = input$customers_sd)
        customers <- max(0, min(round(customers_raw), input$max_customers))
        customers_matrix[day, iter] <- customers

        # Calculate revenue
        entry_fees <- customers * rnorm(1, mean = input$entry_fee, sd = input$entry_fee_sd)

        consumables_revenue <- 0
        if (customers > 0) {
            consumables_counts <- rpois(customers, lambda = input$consumables_mean)
            consumables_prices <- rnorm(customers, mean = input$consumables_price, sd = input$consumables_price_sd)
            consumables_revenue <- sum(consumables_counts * consumables_prices)
        }

        daily_revenue <- max(0, entry_fees) + max(0, consumables_revenue)
        daily_profit <- daily_revenue - daily_exp
        cumulative_cashflow <- cumulative_cashflow + daily_profit

        cashflow_matrix[day, iter] <- cumulative_cashflow

        if (iter == 1) {
            cat("  Day", day, ": customers =", customers, ", revenue =", round(daily_revenue, 2), ", cumulative =", round(cumulative_cashflow, 2), "\n")
        }
    }
}

cat("Mini simulation completed successfully!\n")
cat("Final cashflows:", cashflow_matrix[n_days, ], "\n")
cat("=== TEST COMPLETE ===\n")
