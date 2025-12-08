# Test the monthly aggregation function
library(data.table)
library(lubridate)
source("defaults.R")
source("simulation_engine.R")

# Run a small test simulation
cat("Testing monthly aggregation function...\n")

params <- get_default_simulation_params()
params$n_iterations <- 3 # Small test
params$n_days <- 90 # 3 months

param_dt <- create_complete_parameters_table(params)
results_dt <- run_modern_simulation(
    param_dt = param_dt,
    revenue_items = params$revenue_items,
    expenses = params$expenses,
    n_iterations = params$n_iterations
)

cat("Running monthly aggregation...\n")
monthly_data <- aggregate_monthly_results(results_dt)

cat("Monthly aggregation results:\n")
print(monthly_data[, .(year_month, month_start, mean_revenue, mean_expenses, mean_profit)])

cat("Summary statistics:\n")
cat("Total months:", nrow(monthly_data), "\n")
cat("Average monthly revenue:", round(mean(monthly_data$mean_revenue), 0), "\n")
cat("Average monthly expenses:", round(mean(monthly_data$mean_expenses), 0), "\n")
cat("Average monthly profit:", round(mean(monthly_data$mean_profit), 0), "\n")

cat("Monthly aggregation test completed successfully!\n")
