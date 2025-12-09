# Test simulation execution
library(data.table)
library(lubridate)
source("helpers/defaults.R")
source("helpers/simulation_engine.R")

# Test simulation
params <- get_default_simulation_params()
param_dt <- create_complete_parameters_table(params)

print("Running simulation...")
results_dt <- run_modern_simulation(
  param_dt = param_dt,
  revenue_items = params$revenue_items,
  expenses = params$expenses,
  n_iterations = 5 # Small test
)

print("Simulation results structure:")
print(str(results_dt))

print("Sample results (first 10 rows):")
print(results_dt[1:10, .(date, iteration, actual_customers, actual_total_revenue, actual_total_expenses, daily_cashflow)])

print("Customer summary:")
print(summary(results_dt$actual_customers))

# Test aggregation
print("Testing aggregation...")
aggregated <- aggregate_simulation_results(results_dt)
print("Daily summary structure:")
print(str(aggregated$daily_summary))

print("Sample daily summary:")
print(aggregated$daily_summary[1:5, .(date, mean_customers, mean_cashflow)])
