# Simple test for customer calculation
library(data.table)
library(lubridate)
source("defaults.R")
source("simulation_engine.R")

# Test basic parameter creation
params <- get_default_simulation_params()
param_dt <- create_complete_parameters_table(params)

print("Parameter table structure:")
print(str(param_dt))

print("First 5 rows of parameters:")
print(param_dt[1:5, .(date, base_customers, weekly_multiplier, seasonal_multiplier, combined_multiplier, expected_customers)])

print("Summary statistics:")
print(summary(param_dt$expected_customers))
