# precompute_scenarios.R — Generate all precomputed data for the app
#
# Usage: Rscript scripts/precompute_scenarios.R
# Output: data/precomputed/single_n200.rds              (Single Allocation landing page)
#         data/precomputed/frontier_corr{X}_rebal{Y}_n5000.rds  (15 frontier files)
#
# Grid: 5 correlations × 3 rebalance intervals = 15 scenarios
# Each scenario: 5000 sims × 11 allocations = 55,000 sims
# Total: 825,000 sims (~23 hours)
#
# Run from project root. Can be interrupted and restarted — existing files are skipped.

library(ggplot2)
library(mvtnorm)
library(truncnorm)
library(reshape2)
library(scales)

source("R/helpers.R")
source("R/simulation_engine.R")
source("R/meta_sweep.R")

# --- Scenario grid ---
correlations <- c(0.7, 0.3, 0, -0.3, -0.7)
rebal_intervals <- c(300, 100, 25)
nSims <- 5000

# Shared parameters (everything except correlation and rebal interval)
base_params <- list(
  n               = 1000,
  s.mean          = 10,
  s.sd            = 6,
  b.mean          = 10,
  b.sd            = 3,
  init.inv        = 1000,
  s.int           = 0.06,
  b.int           = 0.02,
  int.period      = 300,
  propStepDivisor = 20
)

sweep_allocs <- clamp_pct_to_fraction(seq(0, 100, by = 10))

dir.create("data/precomputed", recursive = TRUE, showWarnings = FALSE)

# --- Single allocation (for landing page) ---
single_path <- "data/precomputed/single_n200.rds"
if (file.exists(single_path)) {
  cat(sprintf("SKIP %s (already exists)\n\n", single_path))
} else {
  cat("Running single allocation (nSims=200)...\n")
  set.seed(100)
  single_result <- do.call(run_simulation, c(base_params, list(
    s.b.corr       = 0.7,
    rebal.interval = 100,
    perc.stocks    = clamp_pct_to_fraction(60),
    nSims          = 200,
    nSimsToRecord  = 10,
    progress_callback = function(i, total, msg) {
      if (i %% 20 == 0) cat(sprintf("  single: %d/%d\n", i, total))
    }
  )))
  saveRDS(single_result, single_path)
  cat(sprintf("Saved %s\n\n", single_path))
}

# --- Run grid ---
scenarios <- expand.grid(corr = correlations, rebal = rebal_intervals)
total <- nrow(scenarios)

for (i in seq_len(total)) {
  corr <- scenarios$corr[i]
  rebal <- scenarios$rebal[i]

  # File naming: corr as integer string (e.g., 0.7 -> "070", -0.3 -> "n030")
  corr_str <- if (corr < 0) {
    paste0("n", sprintf("%03d", abs(round(corr * 100))))
  } else {
    sprintf("%03d", round(corr * 100))
  }
  fname <- sprintf("data/precomputed/frontier_corr%s_rebal%d_n%d.rds",
                   corr_str, rebal, nSims)

  # Skip if already computed (allows restart after interruption)
  if (file.exists(fname)) {
    cat(sprintf("[%d/%d] SKIP %s (already exists)\n", i, total, fname))
    next
  }

  cat(sprintf("[%d/%d] corr=%.1f, rebal=%d, nSims=%d ...\n",
              i, total, corr, rebal, nSims))

  params <- c(base_params, list(
    s.b.corr        = corr,
    rebal.interval  = rebal,
    perc.stocks.vec = sweep_allocs,
    nSims           = nSims,
    nSimsToRecord   = 10,
    progress_callback = function(alloc_i, total_allocs, sim_i, total_sims, msg) {
      if (sim_i %% 1000 == 0) {
        cat(sprintf("  alloc %d/%d, sim %d/%d\n",
                    alloc_i, total_allocs, sim_i, total_sims))
      }
    }
  ))

  set.seed(100)
  start_time <- Sys.time()
  result <- do.call(run_meta_sweep, params)
  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)

  saveRDS(result, fname)
  cat(sprintf("  Saved %s (%.1f min)\n\n", fname, elapsed))
}

cat("Done. All scenario precomputes saved to data/precomputed/\n")
