# sanity_check_no_rebal.R — Verify rebal vs nonrebal curves are identical
#                           when rebalancing never fires
#
# Usage: Rscript scripts/sanity_check_no_rebal.R
#
# With rebal.interval >> n, no rebalancing events occur.
# Both portfolios start with the same shares and evolve on the same price paths,
# so every output should be identical. Any difference is a bug.

library(mvtnorm)
library(truncnorm)
library(zoo)

source("R/helpers.R")
source("R/simulation_engine.R")
source("R/meta_sweep.R")

cat("=== Sanity Check: No-Rebalancing Identity Test ===\n\n")

set.seed(42)

result <- run_meta_sweep(
  perc.stocks.vec = clamp_pct_to_fraction(seq(0, 100, by = 10)),
  n               = 1000,
  s.mean          = 10,
  s.sd            = 6,
  b.mean          = 10,
  b.sd            = 3,
  s.b.corr        = -0.7,
  init.inv        = 1000,
  rebal.interval  = 99999,   # <-- never fires (99999 >> 999 timesteps)
  s.int           = 0.06,
  b.int           = 0.02,
  int.period      = 300,
  propStepDivisor = 20,
  nSims           = 500,
  nSimsToRecord   = 5,
  widthVal        = 600
)

# --- Check summary (point estimates) ---
s <- result$summary
cat("Allocation | rebal_gain | nonrebal_gain | gain_diff | rebal_sd | nonrebal_sd | sd_diff\n")
cat(strrep("-", 95), "\n")
for (i in seq_len(nrow(s))) {
  cat(sprintf("    %3.0f%%   | %10.6f | %13.6f | %9.2e | %8.2f | %11.2f | %7.2e\n",
              s$allocation[i] * 100,
              s$rebal_gain[i], s$nonrebal_gain[i],
              abs(s$rebal_gain[i] - s$nonrebal_gain[i]),
              s$rebal_sd[i], s$nonrebal_sd[i],
              abs(s$rebal_sd[i] - s$nonrebal_sd[i])))
}

# --- Check cloud (per-sim values) ---
cloud <- result$cloud
allocs <- unique(cloud$allocation)
max_gain_diff <- 0
max_sd_diff <- 0

for (a in allocs) {
  rebal <- cloud[cloud$allocation == a & cloud$type == "rebal", ]
  nonrebal <- cloud[cloud$allocation == a & cloud$type == "nonrebal", ]
  rebal <- rebal[order(rebal$sim), ]
  nonrebal <- nonrebal[order(nonrebal$sim), ]

  gd <- max(abs(rebal$gain - nonrebal$gain))
  sd <- max(abs(rebal$sd - nonrebal$sd))
  if (gd > max_gain_diff) max_gain_diff <- gd
  if (sd > max_sd_diff) max_sd_diff <- sd
}

cat("\n--- Cloud (per-sim) max differences ---\n")
cat(sprintf("  Max |gain_rebal - gain_nonrebal|: %.2e\n", max_gain_diff))
cat(sprintf("  Max |sd_rebal   - sd_nonrebal|:   %.2e\n", max_sd_diff))

tol <- 1e-10
if (max_gain_diff < tol && max_sd_diff < tol) {
  cat("\nPASS: Rebalanced and non-rebalanced curves are identical (diff < 1e-10).\n")
  cat("      All divergence at normal rebal.interval is from the rebalancing mechanism.\n")
} else {
  cat("\nFAIL: Curves differ even with no rebalancing events!\n")
  cat("      There may be a bug in simulate_portfolios().\n")
}
