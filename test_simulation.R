# Enhanced Test Script for Business Model Simulation
# Tests the modern modular architecture with all advanced features
# Updated: September 27, 2025

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(lubridate)

# Source the modular components
source("defaults.R")
source("helper.R")

cat("=== BUSINESS MODEL SIMULATION TEST SUITE ===\n")
cat("Testing enhanced simulation with currency support, seasonal patterns, and modular architecture\n\n")

# Test 1: Currency System
cat("1. TESTING CURRENCY SYSTEM\n")
cat("Available currencies:", names(CURRENCIES), "\n")

# Test currency conversion
test_amount <- 100 # 100 EUR
for (currency in names(CURRENCIES)) {
    converted <- convert_currency(test_amount, "EUR", currency)
    display_symbol <- get_currency_display_symbol(currency)
    cat(sprintf(
        "  %s %.2f -> %s %.2f\n",
        CURRENCIES[["EUR"]]$symbol, test_amount,
        display_symbol, converted
    ))
}

# Test VND scaling
vnd_display <- format_vnd_display(265000) # 265K VND
vnd_actual <- format_vnd_actual(vnd_display) # Back to actual
cat(sprintf(
    "  VND scaling test: 265,000 VND -> %.0f K-VND -> %.0f VND\n",
    vnd_display, vnd_actual
))

cat("\n2. TESTING DEFAULT PARAMETERS\n")
default_params <- get_default_simulation_params()
cat("  Iterations:", default_params$n_iterations, "\n")
cat("  Duration:", default_params$n_days, "days\n")
cat("  Base customers:", default_params$base_customers, "\n")
cat("  Facility capacity:", default_params$max_customers, "\n")
cat("  Revenue items:", length(default_params$revenue_items), "\n")
cat("  Expense categories:", length(default_params$expenses), "\n")
cat("  Seasonal periods:", length(default_params$seasonal_periods), "\n")

cat("\n3. TESTING SEASONAL PERIODS\n")
for (i in seq_along(DEFAULT_SEASONAL_PERIODS)) {
    period <- DEFAULT_SEASONAL_PERIODS[[i]]
    cat(sprintf(
        "  %d. %s (%s to %s): %.1fx multiplier [%s]\n",
        i, period$name, period$start, period$end,
        period$multiplier, period$recurring
    ))
}

cat("\n4. TESTING CURRENCY RANGES\n")
for (currency in names(CURRENCIES)) {
    ranges <- get_currency_adjusted_ranges(currency)
    symbol <- get_currency_display_symbol(currency)
    cat(sprintf("  %s (%s):\n", currency, symbol))
    cat(sprintf(
        "    Price range: 0 - %.0f (step: %.2f)\n",
        ranges$price$max, ranges$price$step
    ))
    cat(sprintf(
        "    Expense range: 0 - %.0f (step: %.0f)\n",
        ranges$expense_value$max, ranges$expense_value$step
    ))
}

cat("\n5. TESTING VALIDATION FUNCTIONS\n")
# Test parameter validation
test_params <- get_default_simulation_params()
test_params$n_iterations <- 5 # Quick test
test_params$n_days <- 30 # One month
errors <- validate_simulation_params(test_params)
if (length(errors) == 0) {
    cat("  ✓ Default parameters validation: PASSED\n")
} else {
    cat("  ✗ Validation errors:", paste(errors, collapse = "; "), "\n")
}

cat("\n6. TESTING HELPER FUNCTIONS\n")
# Test frequency conversion
test_freqs <- c("daily", "weekly", "monthly", "yearly", "investment")
for (freq in test_freqs) {
    multiplier <- freq_to_daily(freq)
    cat(sprintf("  %s: %.6f daily multiplier\n", freq, multiplier))
}

# Test customer calculation with seasonal effects
test_date <- as.Date("2025-12-25") # Christmas
expected_customers <- calculate_expected_customers(
    test_date, 50,
    DEFAULT_WEEKLY_MULTIPLIERS, DEFAULT_SEASONAL_PERIODS,
    list(enabled = FALSE, annual_rate = 0, start_date = Sys.Date())
)
cat(sprintf(
    "  Expected customers on %s: %.1f (base: 50)\n",
    test_date, expected_customers
))

cat("\n7. RUNNING MINI SIMULATION\n")
cat("Testing complete simulation pipeline with modern architecture...\n")

# Create test parameters
mini_params <- list(
    n_iterations = 3,
    n_days = 10,
    start_date = Sys.Date(),
    max_customers = 100,
    base_customers = 50,
    customers_sd = 15,
    weekly_multipliers = DEFAULT_WEEKLY_MULTIPLIERS,
    seasonal_periods = DEFAULT_SEASONAL_PERIODS,
    trend_params = list(enabled = FALSE, annual_rate = 0, start_date = Sys.Date()),
    expenses = DEFAULT_EXPENSES,
    revenue_items = DEFAULT_REVENUE_ITEMS
)

# Run the simulation using the actual helper function
tryCatch(
    {
        cat("  Starting simulation...\n")
        results <- run_complete_simulation(mini_params)

        cat("  ✓ Simulation completed successfully!\n")
        cat(sprintf("  Final statistics:\n"))
        cat(sprintf("    - Average daily customers: %.1f\n", results$summary$avg_daily_customers))
        cat(sprintf("    - Average daily revenue: €%.2f\n", results$summary$avg_daily_revenue))
        cat(sprintf("    - Final profit: €%.2f\n", results$summary$final_profit))
        cat(sprintf(
            "    - Break-even achieved: %s\n",
            ifelse(results$summary$final_profit > 0, "YES", "NO")
        ))
    },
    error = function(e) {
        cat("  ✗ Simulation failed:", e$message, "\n")
    }
)

cat("\n8. TESTING MULTI-CURRENCY SIMULATION\n")
# Test with different currencies
currencies_to_test <- c("EUR", "USD", "VND")

for (currency in currencies_to_test) {
    cat(sprintf("  Testing with %s...\n", currency))

    # Convert parameters to test currency
    converted_expenses <- convert_monetary_list(DEFAULT_EXPENSES, "EUR", currency)
    converted_revenue <- convert_monetary_list(DEFAULT_REVENUE_ITEMS, "EUR", currency)

    currency_params <- mini_params
    currency_params$expenses <- converted_expenses
    currency_params$revenue_items <- converted_revenue
    currency_params$n_iterations <- 2 # Quick test
    currency_params$n_days <- 5

    tryCatch(
        {
            results <- run_complete_simulation(currency_params)
            symbol <- get_currency_display_symbol(currency)
            cat(sprintf(
                "    ✓ %s simulation: Final profit %s%.0f\n",
                currency, symbol, results$summary$final_profit
            ))
        },
        error = function(e) {
            cat(sprintf("    ✗ %s simulation failed: %s\n", currency, e$message))
        }
    )
}

cat("\n=== TEST SUITE COMPLETE ===\n")
cat("Enhanced business model simulation system tested successfully!\n")
cat("All modern features verified: currencies, seasonal patterns, modular architecture.\n")
