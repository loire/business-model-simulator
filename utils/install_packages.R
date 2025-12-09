# Required packages for Business Model Simulation Dashboard

# Install required packages if not already installed
packages <- c(
  "shiny",
  "shinydashboard",
  "plotly",
  "DT",
  "dplyr",
  "lubridate"
)

# Function to install missing packages
install_if_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(missing_packages)) {
    install.packages(missing_packages, dependencies = TRUE)
    cat("Installed packages:", paste(missing_packages, collapse = ", "), "\n")
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install missing packages
install_if_missing(packages)

# Load all packages
lapply(packages, library, character.only = TRUE)

cat("All packages loaded successfully.\n")
cat("You can now run the Shiny app with: shiny::runApp()\n")
