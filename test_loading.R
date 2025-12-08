# Test loading functionality
library(data.table)
source("simulation_engine.R")

# Test that we can load the latest files
cat("Testing file loading capability...\n")

# Check if files exist
if (file.exists("latest_simulation.csv") && file.exists("latest_parameters.csv")) {
    cat("✓ Latest files exist\n")

    # Test loading
    loaded_results <- load_results_table("latest_simulation.csv")
    loaded_parameters <- load_parameters_table("latest_parameters.csv")

    cat("✓ Loaded", nrow(loaded_results), "simulation rows\n")
    cat("✓ Loaded", nrow(loaded_parameters), "parameter rows\n")

    # Test aggregation
    aggregated <- aggregate_simulation_results(loaded_results)
    cat("✓ Aggregated results into", nrow(aggregated$daily_summary), "daily summaries\n")

    # Check data types
    cat("✓ Simulation results date range:", min(loaded_results$date), "to", max(loaded_results$date), "\n")
    cat("✓ Average daily customers:", round(mean(aggregated$daily_summary$mean_customers), 1), "\n")
    cat("✓ Final cash flow:", round(aggregated$summary_stats$final_profit, 0), "\n")

    cat("\n🎉 Loading test completed successfully!\n")
} else {
    cat("❌ Latest files not found\n")
}
