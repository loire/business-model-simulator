# defaults.R - Default values for all input variables
# Centralized configuration for the Business Model Simulation

# Default simulation parameters
DEFAULT_SIMULATION <- list(
    n_iterations = 10,
    n_days = 365 * 3, # 3 years
    start_date = Sys.Date()
)

# Default facility parameters
DEFAULT_FACILITY <- list(
    max_customers = 50,
    base_customers = 10,
    customers_sd = 4
)

# Default day-of-week multipliers (1=Monday, 7=Sunday)
DEFAULT_WEEKLY_MULTIPLIERS <- c(
    Monday = 0.8, # Slower start to week
    Tuesday = 0.9, # Building up
    Wednesday = 1.0, # Average
    Thursday = 1.0, # Average
    Friday = 1.3, # Peak day
    Saturday = 1.5, # Weekend peak
    Sunday = 1.1 # Weekend but slower
)

# Default seasonal periods (holidays/special events)
DEFAULT_SEASONAL_PERIODS <- list(
    list(
        name = "Christmas Period",
        start = "12-20",
        end = "12-31",
        multiplier = 1.8,
        recurring = "yearly",
        description = "Holiday season boost"
    ),
    list(
        name = "Summer Vacation",
        start = "07-01",
        end = "08-31",
        multiplier = 1.2,
        recurring = "yearly",
        description = "Summer tourist season"
    ),
    list(
        name = "Back to School",
        start = "09-01",
        end = "09-15",
        multiplier = 0.7,
        recurring = "yearly",
        description = "Lower activity during school start"
    ),
    list(
        name = "Tet Holiday (Vietnamese New Year)",
        start = "02-08", # Approximate - varies by lunar calendar
        end = "02-14",
        multiplier = 2.5,
        recurring = "yearly",
        description = "Vietnamese New Year celebration - major holiday"
    ),
    list(
        name = "Easter Period",
        start = "03-29", # Approximate - varies by year
        end = "04-05",
        multiplier = 1.4,
        recurring = "yearly",
        description = "Easter holiday period"
    )
)

# Default trend parameters
DEFAULT_TREND <- list(
    enabled = FALSE,
    annual_rate = 0.05, # 5% annual growth
    start_date = Sys.Date()
)

# Default investment timing
DEFAULT_INVESTMENT_TIMING <- list(
    day = 1, # Day of simulation when investment occurs (1 = first day)
    description = "Initial capital investment on first day of operations"
)

# Currency configuration
DEFAULT_CURRENCY <- "EUR"

CURRENCIES <- list(
    "EUR" = list(
        symbol = "€",
        name = "Euro",
        rate = 1.0 # Base currency
    ),
    "USD" = list(
        symbol = "$",
        name = "US Dollar",
        rate = 1.10 # 1 EUR = 1.10 USD (approximate)
    ),
    "VND" = list(
        symbol = "₫",
        name = "Vietnamese Dong",
        rate = 26500 # 1 EUR = 26,500 VND (approximate)
    )
)

# Period recurrence types
RECURRENCE_TYPES <- list(
    "One-time only" = "once",
    "Repeat every year" = "yearly"
)

# Default expense categories
DEFAULT_EXPENSES <- list(
    rent = list(
        name = "Rent",
        value = 2000,
        sd = 0, # Changed from 200 to 0
        freq = "monthly",
        description = "Facility rental costs"
    ),
    taxes = list(
        name = "Taxes",
        value = 5000,
        sd = 0, # Changed from 500 to 0
        freq = "yearly",
        description = "Annual tax obligations"
    ),
    consumables_cost = list(
        name = "Consumables Supply",
        value = 300,
        sd = 0, # Changed from 50 to 0
        freq = "weekly",
        description = "Weekly consumable supply costs"
    ),
    it = list(
        name = "IT Infrastructure",
        value = 2000,
        sd = 0, # Changed from 300 to 0
        freq = "yearly",
        description = "Annual IT infrastructure costs"
    ),
    material = list(
        name = "Material Investment",
        value = 15000,
        sd = 0, # Changed from 2000 to 0
        freq = "investment",
        description = "One-time material investment"
    ),
    staff = list(
        name = "Staff Salaries",
        value = 4000,
        sd = 0, # Changed from 400 to 0
        freq = "monthly",
        description = "Monthly staff salary costs"
    )
)

# Default revenue items
DEFAULT_REVENUE_ITEMS <- list(
    entry_fee = list(
        name = "Entry Fee",
        type = "fixed_per_customer",
        price = 15,
        price_sd = 0, # Changed from 2 to 0
        description = "Fixed entry fee per customer"
    ),
    consumables = list(
        name = "Consumables Sales",
        type = "poisson_per_customer",
        lambda = 1,
        price = 3,
        price_sd = 0, # Changed from 0.5 to 0
        description = "Consumables purchased per customer (Poisson)"
    ),
    premium_service = list(
        name = "Premium Service",
        type = "probability_per_customer",
        probability = 0.3,
        lambda = 1,
        price = 20,
        price_sd = 0, # Changed from 3 to 0
        description = "30% chance customer buys premium service"
    )
)

# Frequency options for expenses
FREQUENCY_OPTIONS <- list(
    "Daily" = "daily",
    "Weekly" = "weekly",
    "Monthly" = "monthly",
    "Yearly" = "yearly",
    "One-time Investment" = "investment"
)

# Revenue item types
REVENUE_TYPES <- list(
    "Fixed per Customer" = "fixed_per_customer",
    "Poisson per Customer" = "poisson_per_customer",
    "Probability per Customer" = "probability_per_customer"
)

# Days of week for display
DAYS_OF_WEEK <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# Slider ranges
SLIDER_RANGES <- list(
    customers = list(min = 1, max = 500, step = 1),
    customers_sd = list(min = 0, max = 100, step = 1),
    max_customers = list(min = 10, max = 1000, step = 10),
    price = list(min = 0, max = 100, step = 0.5),
    price_sd = list(min = 0, max = 20, step = 0.1),
    probability = list(min = 0, max = 1, step = 0.01),
    lambda = list(min = 0.1, max = 10, step = 0.1),
    weekly_multiplier = list(min = 0.1, max = 3.0, step = 0.1),
    seasonal_multiplier = list(min = 0.1, max = 5.0, step = 0.1),
    trend_rate = list(min = -0.5, max = 0.5, step = 0.01),
    expense_value = list(min = 0, max = 50000, step = 100),
    expense_sd = list(min = 0, max = 5000, step = 50)
)

# Function to get default parameter list for simulation
get_default_simulation_params <- function() {
    return(list(
        n_iterations = DEFAULT_SIMULATION$n_iterations,
        n_days = DEFAULT_SIMULATION$n_days,
        start_date = DEFAULT_SIMULATION$start_date,
        max_customers = DEFAULT_FACILITY$max_customers,
        base_customers = DEFAULT_FACILITY$base_customers,
        customers_sd = DEFAULT_FACILITY$customers_sd,
        weekly_multipliers = DEFAULT_WEEKLY_MULTIPLIERS,
        seasonal_periods = DEFAULT_SEASONAL_PERIODS,
        trend_params = DEFAULT_TREND,
        investment_timing = DEFAULT_INVESTMENT_TIMING,
        expenses = DEFAULT_EXPENSES,
        revenue_items = DEFAULT_REVENUE_ITEMS
    ))
}

# Help text and tooltips for UI components
HELP_TEXT <- list(
    # Main dashboard
    facility_capacity = "Maximum number of customers your facility can accommodate at one time. This acts as a hard limit on daily customer flow.",
    base_customers = "Expected number of customers per day under normal conditions, without seasonal effects or day-of-week variations.",
    customer_variability = "Standard deviation for daily customer count variation. Higher values create more unpredictable customer flow (keep this >0 for realistic simulation).",
    currency = "Select your preferred currency for all financial calculations and displays. Conversion rates are applied automatically.",
    n_iterations = "Number of simulation runs to perform. More iterations provide more reliable statistical results but take longer to compute.",
    n_years = "Duration of the business simulation in years. Longer periods help assess long-term viability and seasonal patterns.",

    # Revenue items
    entry_fee = "Fixed amount charged to each customer for basic access to your service or facility.",
    consumables_sales = "Additional purchases customers make (food, drinks, merchandise). Uses Poisson distribution for realistic randomness.",
    premium_service = "Optional higher-value services that only some customers purchase. Set probability and price accordingly.",

    # Expenses
    rent = "Regular facility rental costs. Usually paid monthly.",
    taxes = "Annual tax obligations including business taxes, property taxes, etc.",
    consumables_cost = "Cost to replenish consumable items sold to customers (inventory costs).",
    it_infrastructure = "Technology costs including software licenses, hardware, internet, etc.",
    material_investment = "One-time capital investment for equipment, furniture, initial setup costs.",
    staff_salaries = "Regular employee compensation including salaries, benefits, and payroll taxes.",

    # Seasonal patterns
    seasonal_periods = "Special time periods with different customer behavior (holidays, events, seasonal trends).",
    weekly_multipliers = "Different customer patterns for each day of the week (e.g., weekends typically busier).",

    # Graphs
    customer_flow_graph = "Shows projected daily customer counts over time with statistical confidence intervals (25%-75% range).",
    cashflow_graph = "Displays cumulative cash flow progression. Green dashed line marks break-even point when business becomes profitable.",
    monthly_profit_graph = "Monthly breakdown of revenues vs expenses showing profit/loss trends. Helps identify seasonal patterns and financial cycles.",
    time_range_controls = "Use slider and buttons to focus on specific time periods. Drag slider handles or click preset buttons (30D, 3M, etc.).",

    # Results
    avg_daily_customers = "Average number of customers per day across all simulation iterations.",
    avg_daily_revenue = "Average daily income from all revenue sources combined.",
    total_profit = "Final cumulative profit/loss after all expenses and revenue over the entire simulation period.",

    # Investment timing
    investment_timing = "Specify which day of the simulation the one-time investment expenses should be applied. Day 1 = first day of operations."
)
