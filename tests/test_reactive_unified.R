# Test script to verify unified reactive system
library(data.table)
library(lubridate)
source("helpers/defaults.R")
source("helpers/simulation_engine.R")

cat("=== TESTING UNIFIED REACTIVE SYSTEM ===\n")

# Simulate the inputs that would come from Shiny
mock_inputs <- list(
    weekly_monday = 1.0,
    weekly_tuesday = 0.8,
    weekly_wednesday = 0.9,
    weekly_thursday = 1.0,
    weekly_friday = 1.2,
    weekly_saturday = 1.5,
    weekly_sunday = 1.3,
    max_customers = 100,
    base_customers = 50,
    customers_sd = 15,
    n_iterations = 10,
    n_years = 3,
    trend_enabled = FALSE,
    annual_growth_rate = 0.05
)

# Test the create_complete_parameters_table function directly
params <- list(
    n_iterations = mock_inputs$n_iterations,
    n_days = mock_inputs$n_years * 365,
    start_date = Sys.Date(),
    max_customers = mock_inputs$max_customers,
    base_customers = mock_inputs$base_customers,
    customers_sd = mock_inputs$customers_sd,
    weekly_multipliers = c(
        mock_inputs$weekly_monday,
        mock_inputs$weekly_tuesday,
        mock_inputs$weekly_wednesday,
        mock_inputs$weekly_thursday,
        mock_inputs$weekly_friday,
        mock_inputs$weekly_saturday,
        mock_inputs$weekly_sunday
    ),
    seasonal_periods = DEFAULT_SEASONAL_PERIODS,
    trend_params = list(enabled = FALSE, annual_rate = 0.05, start_date = Sys.Date()),
    investment_dates = list(),
    expenses = DEFAULT_EXPENSES,
    revenue_items = DEFAULT_REVENUE_ITEMS
)

cat("Creating parameters table...\n")
param_dt <- create_complete_parameters_table(params)

cat("Parameters table created successfully!\n")
cat("Rows:", nrow(param_dt), "\n")
cat("Columns:", ncol(param_dt), "\n")
cat("Expected customers range:", min(param_dt$expected_customers), "to", max(param_dt$expected_customers), "\n")

# Test calendar data calculation
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2025-12-31")
all_dates <- seq(start_date, end_date, by = "day")

combined_multipliers <- sapply(all_dates, function(date) {
    seasonal_mult <- calculate_seasonal_multiplier(date, DEFAULT_SEASONAL_PERIODS)
    day_of_week <- lubridate::wday(date)
    weekly_mult <- params$weekly_multipliers[ifelse(day_of_week == 1, 7, day_of_week - 1)]
    return(seasonal_mult * weekly_mult)
})

cat("Calendar multipliers calculated successfully!\n")
cat("Multiplier range:", round(min(combined_multipliers), 2), "to", round(max(combined_multipliers), 2), "\n")
cat("Average multiplier:", round(mean(combined_multipliers), 2), "\n")

# Test diverging color scale logic
min_val <- min(combined_multipliers)
max_val <- max(combined_multipliers)
mid_val <- 1.0
range_below <- mid_val - min_val
range_above <- max_val - mid_val
max_range <- max(range_below, range_above)

legend_values <- c(
    round(mid_val - max_range, 2),
    round(mid_val - max_range / 2, 2),
    mid_val,
    round(mid_val + max_range / 2, 2),
    round(mid_val + max_range, 2)
)

cat("Diverging color scale legend values:", paste(legend_values, collapse = ", "), "\n")

cat("\n=== TEST COMPLETED SUCCESSFULLY ===\n")
