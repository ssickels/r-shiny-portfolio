# Monte Carlo Portfolio Simulator

An interactive Shiny app that uses Metropolis-Hastings MCMC to simulate correlated stock and bond price paths, comparing rebalanced vs non-rebalanced portfolio strategies across the efficient frontier.

**[Live demo](https://ssickels.shinyapps.io/r-shiny-portfolio/)**

## What it does

The simulator generates thousands of correlated stock/bond price paths using a Metropolis-Hastings sampler, then computes portfolio values under two strategies: periodic rebalancing back to a target allocation, and buy-and-hold (no rebalancing). The emphasis is on distributions, not point estimates: every plot shows the full spread of outcomes.

Two main modes:

- **Single Allocation** — run many simulations at one stock/bond split. Explore outcomes via gain vs. SD clouds, final-value densities, price trajectories, cost profiles, and a single-sim explorer that unpacks any individual run.
- **Frontier Sweep** — sweep across all allocations to trace *two* efficient frontiers (rebalanced vs. non-rebalanced), revealing how rebalancing reshapes the risk-return tradeoff. Step through allocations with arrow keys, compare scenarios across correlation levels and rebalancing intervals via a precomputed scenario grid, and adjust cloud opacity or zoom to isolate details.

## Project history

- **2021–22** — Original R scripts exploring Monte Carlo portfolio simulation (preserved in `original_scripts/`)
- **2025** — Refactored into modular pure functions; built the Shiny interactive app
- Functions extracted from Shiny into `R/` modules for console and RStudio use
- Precomputed frontier datasets (5,000 sims each) across 4 correlation levels (−0.3, 0, 0.3, 0.7) and 3 rebalancing intervals (25, 100, 300 days) for instant scenario comparison

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
├── data/precomputed/          # .rds frontier datasets (correlation × rebalancing grid)
├── scripts/                   # Batch scripts (e.g., precompute_scenarios.R)
├── original_scripts/          # Legacy monolithic R scripts (2021–22)
├── docs/                      # Architecture, flowchart, console recipes
├── site/                      # Companion HTML pages + "Behind the Assumptions" essays
│   ├── img/                   # Screenshots and figures for site pages
│   ├── portfolio-nav.js       # Shared navigation bar (tabs + hamburger menu)
│   ├── behind-the-assumptions.html  # Landing page with animated cartoon
│   └── behind-{1..4}-*.html   # Chapter pages with interactive widgets
└── www/                       # Shiny static assets (CSS)
```

## Site pages

Companion pages on [stevessite.com](https://stevessite.com):

- [Simulator](https://stevessite.com/monte-carlo.html) — Shiny app embed
- [Getting Started](https://stevessite.com/monte-carlo-guide.html)
- [About](https://stevessite.com/monte-carlo-about.html)
- [How It Works](https://stevessite.com/monte-carlo-methodology.html)

### Behind the Assumptions

A four-chapter essay section examining the uncertainty *behind* the simulator's inputs — the question of what parameter values to use in the first place. Includes interactive widgets and an animated cartoon.

- [Landing page](https://stevessite.com/behind-the-assumptions.html) — overview, embedded two-pipelines animation, chapter previews
- [Ch 1: The "Optimal" Portfolio](https://stevessite.com/behind-1-optimal.html) — Sharpe ratio, tangency portfolios, and why the textbook answer is fragile
- [Ch 2: Parameter Uncertainty](https://stevessite.com/behind-2-parameter-uncertainty.html) — interactive two-asset widget showing how input uncertainty propagates to the "optimal" allocation
- [Ch 3: The Inputs Aren't Stable](https://stevessite.com/behind-3-drift.html) — historical evidence that means, volatilities, and correlations drift over time
- [Ch 4: Three Assets and Beyond](https://stevessite.com/behind-4-three-assets.html) — interactive three-asset ternary widget, section coda
