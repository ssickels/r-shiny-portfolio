# precompute_frontier.R — Run locally to generate .rds files for app startup
#
# Usage: Rscript scripts/precompute_frontier.R
# Output: data/precomputed/frontier_n500.rds, frontier_n2000.rds,
#         frontier_n5000.rds, single_n200.rds

library(ggplot2)
library(mvtnorm)
library(truncnorm)
library(reshape2)
library(zoo)
library(scales)

source("R/helpers.R")
source("R/simulation_engine.R")
source("R/meta_sweep.R")

# Shared parameters (asymmetric assets)
shared_params <- list(
  n            = 1000,
  s.mean       = 10,
  s.sd         = 6,
  b.mean       = 10,
  b.sd         = 3,
  s.b.corr     = 0.7,
  init.inv     = 1000,
  rebal.interval = 100,
  s.int        = 0.06,
  b.int        = 0.02,
  int.period   = 300,
  propStepDivisor = 20,
  widthVal     = 600
)

sweep_allocs <- clamp_pct_to_fraction(seq(0, 100, by = 10))

# --- Single allocation (for landing page) ---
cat("Running single allocation (nSims=200)...\n")
set.seed(100)
single_result <- do.call(run_simulation, c(shared_params, list(
  perc.stocks    = clamp_pct_to_fraction(60),
  nSims          = 200,
  nSimsToRecord  = 10,
  progress_callback = function(i, total, msg) {
    if (i %% 20 == 0) cat(sprintf("  single: %d/%d\n", i, total))
  }
)))
saveRDS(single_result, "data/precomputed/single_n200.rds")
cat("Saved data/precomputed/single_n200.rds\n\n")

# --- Frontier sweeps ---
for (nSims in c(500, 2000, 5000)) {
  cat(sprintf("Running frontier sweep (nSims=%d)...\n", nSims))
  set.seed(100)
  sweep_result <- do.call(run_meta_sweep, c(shared_params, list(
    perc.stocks.vec = sweep_allocs,
    nSims           = nSims,
    nSimsToRecord   = 5,
    progress_callback = function(alloc_i, total_allocs, sim_i, total_sims, msg) {
      if (sim_i %% max(1, total_sims %/% 5) == 0) {
        cat(sprintf("  alloc %d/%d, sim %d/%d\n",
                    alloc_i, total_allocs, sim_i, total_sims))
      }
    }
  )))
  fname <- sprintf("data/precomputed/frontier_n%d.rds", nSims)
  saveRDS(sweep_result, fname)
  cat(sprintf("Saved %s\n\n", fname))
}

cat("Done. All precomputed data saved to data/precomputed/\n")
