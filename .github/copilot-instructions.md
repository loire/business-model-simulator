<!-- Use this file to provide workspace-specific custom instructions to Copilot. For more details, visit https://code.visualstudio.com/docs/copilot/copilot-customization#_use-a-githubcopilotinstructionsmd-file -->

## Project Setup Progress

- [x] Verify that the copilot-instructions.md file in the .github directory is created.
- [x] Clarify Project Requirements
- [x] Scaffold the Project
- [x] Customize the Project
- [x] Install Required Extensions
- [x] Compile the Project
- [x] Create and Run Task
- [x] Launch the Project
- [x] Ensure Documentation is Complete

## Project Requirements Summary
This is a Shiny dashboard for business model simulation with:
- Dynamic inputs for expenses (recurring: daily/monthly/yearly or investment: one-time)
- Default variables: rent (monthly), taxes (yearly), consumables (weekly), IT (yearly), material (investment), staff (monthly)
- Income calculation based on customers per day with entry fee and consumables (Poisson distribution)
- 3-year time series simulation (100 iterations) for customer count and cash flow
- Interactive graphs displaying results
- User input for facility size (max customers) and variable standard deviations