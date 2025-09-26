# Business Model Simulation Dashboard

A comprehensive R Shiny application for simulating and analyzing business model scenarios over a 3-year period with Monte Carlo simulations.

## Features

- **Dynamic Input Parameters**: Configure expenses (recurring or investment) and income variables
- **Monte Carlo Simulation**: Run 100 iterations of 3-year time series simulations
- **Interactive Visualizations**: Real-time plotting of customer flow and cash flow
- **Default Business Variables**: Pre-configured with common business expenses
- **Facility Management**: Set maximum customer capacity constraints
- **Statistical Analysis**: Confidence intervals and summary statistics

## Default Variables

### Expenses
- **Rent** (Monthly): Facility rental costs
- **Taxes** (Yearly): Annual tax obligations  
- **Consumables** (Weekly): Weekly consumable supply costs
- **IT** (Yearly): Annual IT infrastructure costs
- **Material** (Investment): One-time material investment
- **Staff** (Monthly): Monthly staff salaries

### Income Sources
- **Entry Fee**: Fixed fee per customer (with variability)
- **Consumables Sales**: Random consumables purchased per customer (Poisson distribution)

## Installation

1. **Install R and RStudio** (if not already installed)
2. **Install required packages**:
   ```r
   source("install_packages.R")
   ```

## Required R Packages

- `shiny`: Web application framework
- `shinydashboard`: Dashboard UI components
- `plotly`: Interactive plotting
- `DT`: Interactive data tables
- `dplyr`: Data manipulation
- `lubridate`: Date/time handling

## Usage

1. **Launch the application**:
   ```r
   shiny::runApp()
   ```
   Or open `app.R` in RStudio and click "Run App"

2. **Configure Parameters**:
   - Set facility size (maximum customers)
   - Adjust income parameters (entry fee, consumables)
   - Configure customer flow patterns
   - Modify expense variables in the Variables tab

3. **Run Simulation**:
   - Click "Run Simulation" to generate 100 Monte Carlo iterations
   - View results in real-time interactive plots
   - Analyze 3-year projections with confidence intervals

## Application Structure

```
├── app.R              # Main application entry point
├── ui.R               # User interface definition
├── server.R           # Server logic and simulation engine
├── install_packages.R # Package installation script
└── README.md          # This documentation
```

## Simulation Details

### Customer Generation
- Daily customer count follows normal distribution
- Bounded by facility maximum capacity
- Configurable mean and standard deviation

### Revenue Calculation
- **Entry Fees**: Normal distribution around base entry fee
- **Consumables**: Poisson-distributed purchases per customer
- **Daily Revenue** = (Customers × Entry Fee) + (Total Consumables Sales)

### Expense Calculation
- **Recurring Expenses**: Converted to daily rates based on frequency
- **Investment Expenses**: Applied as one-time costs at simulation start
- **Daily Profit** = Daily Revenue - Daily Expenses

### Time Series Output
- **Customer Flow**: Daily customer counts over 3 years
- **Cash Flow**: Cumulative profit/loss over 3 years
- **Statistics**: Mean, 25th percentile, 75th percentile for each day

## Customization

### Adding New Variables
1. Add input controls in `ui.R`
2. Update calculation logic in `server.R`
3. Modify the expense calculation functions

### Changing Simulation Parameters
- Modify `n_iterations` (default: 100) for more/fewer Monte Carlo runs
- Adjust `n_days` (default: 1095) for different time horizons
- Update distribution parameters for different risk profiles

## Output Interpretation

### Customer Flow Graph
- Shows daily customer count variability
- Shaded area represents 25th-75th percentile range
- Blue line shows mean expected customers

### Cash Flow Graph  
- Displays cumulative profit/loss over time
- Orange line shows mean scenario
- Shaded area shows range of probable outcomes

### Summary Metrics
- **Average Daily Customers**: Mean customer count across all simulations
- **Average Daily Revenue**: Mean daily income generation
- **3-Year Profit**: Final cumulative profit after 3 years

## Technical Notes

- Simulation uses vectorized R operations for performance
- Progress bars indicate simulation status
- Input validation prevents invalid parameter combinations
- Responsive design works on desktop and tablet devices

## Troubleshooting

### Common Issues
1. **Package Installation Errors**: Run `install_packages.R` to resolve dependencies
2. **Memory Issues**: Reduce number of iterations or time period for large simulations
3. **Plot Rendering**: Ensure `plotly` package is properly installed

### Performance Tips
- Start with default parameters to verify functionality
- Reduce iterations for faster initial testing
- Use reasonable parameter ranges to avoid extreme scenarios

## License

This project is open source. Feel free to modify and distribute according to your needs.