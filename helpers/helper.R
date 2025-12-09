# helper.R - Helper functions for Business Model Simulation
# Contains UI helpers and utility functions only
# Simulation functions moved to simulation_engine.R to avoid conflicts

# UI Helper functions
create_help_icon <- function(help_key) {
    help_text <- HELP_TEXT[[help_key]]
    if (is.null(help_text)) {
        return("")
    }

    tags$span(
        icon("question-circle", class = "help-icon"),
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        `data-html` = "true",
        title = help_text,
        style = "margin-left: 5px; color: #007bff; cursor: help;"
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
        label_with_help <- paste(label_with_help, paste0("(", currency_symbol, ")"))
    }

    div(
        tags$label(label_with_help),
        fluidRow(
            column(8,
                sliderInput(inputId, NULL, min = min, max = max, value = value, step = step)
            ),
            column(4,
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

# Function to get day of week (1=Monday, 7=Sunday)
get_day_of_week <- function(date) {
    as.numeric(format(date, "%u"))
}

# Helper function to validate simulation parameters
validate_simulation_params <- function(params) {
    errors <- character(0)
    
    if (params$n_iterations <= 0) {
        errors <- c(errors, "Number of iterations must be positive")
    }
    if (params$n_days <= 0) {
        errors <- c(errors, "Number of days must be positive")
    }
    if (params$max_customers <= 0) {
        errors <- c(errors, "Maximum customers must be positive")
    }
    if (params$base_customers < 0) {
        errors <- c(errors, "Base customers cannot be negative")
    }
    if (params$customers_sd < 0) {
        errors <- c(errors, "Customer standard deviation cannot be negative")
    }
    if (length(params$weekly_multipliers) != 7) {
        errors <- c(errors, "Weekly multipliers must have exactly 7 values")
    }
    
    return(errors)
}

# IMPORTANT: All simulation functions have been moved to simulation_engine.R
# This includes:
# - calculate_seasonal_multiplier()
# - calculate_expected_customers()
# - generate_daily_customers()
# - calculate_custom_revenue()
# - run_complete_simulation()
# - All expense calculation functions
#
# This separation prevents function conflicts and creates clear module boundaries.