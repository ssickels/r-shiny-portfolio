# Monte Carlo Portfolio Simulator

An interactive Shiny app that uses Metropolis-Hastings MCMC to simulate correlated stock and bond price paths, comparing rebalanced vs non-rebalanced portfolio strategies across the efficient frontier.

**[Live demo](https://ssickels.shinyapps.io/r-shiny-portfolio/)**

## What it does

The simulator generates thousands of correlated stock/bond price paths using a Metropolis-Hastings sampler, then computes portfolio values under two strategies: periodic rebalancing back to a target allocation, and buy-and-hold (no rebalancing). By sweeping across allocations from 0% to 100% stocks, it traces out *two* efficient frontiers — one for each strategy — revealing how rebalancing reshapes the risk-return tradeoff. The emphasis is on distributions, not point estimates: every plot shows the full spread of outcomes.

## Project history

- **2021–22** — Original R scripts exploring Monte Carlo portfolio simulation (preserved in `original_scripts/`)
- **2025** — Refactored into modular pure functions; built the Shiny interactive app
- Functions extracted from Shiny into `R/` modules for console and RStudio use
- Precomputed frontier datasets at 500 / 2,000 / 5,000 simulations for instant loading

## Documentation

- **[Architecture & Refactoring](docs/architecture.html)** — How a monolithic script became five modules
- **[Function Dependency Flowchart](docs/dependency-flowchart.html)** — Visual data flow through all functions (interactive SVG with tooltips)
- **[Console Recipes](docs/console-recipes.html)** — Copy-paste code to run simulations step-by-step in R

## Quick start

```r
# Clone and open in RStudio
git clone https://github.com/ssickels/r-shiny-portfolio.git
setwd("r-shiny-portfolio")

# Install dependencies
install.packages(c("shiny", "shinyjs", "ggplot2", "plotly",
                    "mvtnorm", "truncnorm", "zoo", "scales"))

# Run the app
shiny::runApp()
```

Or run simulations from the R console without Shiny:

```r
source("R/helpers.R")
source("R/simulation_engine.R")

result <- run_simulation(
  n = 1000, s.mean = 10, s.sd = 6, b.mean = 10, b.sd = 3,
  s.b.corr = 0.7, init.inv = 1000,
  perc.stocks = clamp_pct_to_fraction(60),
  rebal.interval = 100,
  s.int = 0.06, b.int = 0.02, int.period = 300,
  propStepDivisor = 20, nSims = 500, nSimsToRecord = 5,
  widthVal = 600
)

format_summary_table(result$gainsVsSD.point.estDf)
```

## Project structure

```
r-shiny-portfolio/
├── R/
│   ├── helpers.R              # Utility functions (unit conversion, formatting)
│   ├── simulation_engine.R    # Core MCMC simulation and post-processing
│   ├── meta_sweep.R           # Frontier sweep loop
│   ├── plotting.R             # 8 plot_*() visualization functions
│   └── theme_config.R         # Centralized color/size theme
├── server.R                   # Shiny server (reactivity + delegation)
├── ui.R                       # Shiny UI layout
├── global.R                   # App startup (source modules, load precomputed data)
├── data/precomputed/          # .rds frontier datasets at various sim counts
├── scripts/                   # Batch scripts (e.g., precompute_scenarios.R)
├── original_scripts/          # Legacy monolithic R scripts (2021–22)
├── docs/                      # Architecture, flowchart, console recipes
├── site/                      # Companion HTML pages for stevessite.com
└── www/                       # Shiny static assets (CSS)
```

## Site pages

Companion pages on [stevessite.com](https://stevessite.com):

- [Simulator](https://stevessite.com/portfolio/monte-carlo.html) — Shiny app embed
- [About](https://stevessite.com/portfolio/monte-carlo-about.html)
- [How It Works](https://stevessite.com/portfolio/monte-carlo-methodology.html)
- [Interactive Frontier](https://stevessite.com/portfolio/monte-carlo-frontier.html)
