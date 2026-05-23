# meta_sweep.R — Efficient frontier sweep across allocations

#' Run simulation across a vector of stock/bond allocations
#'
#' @param perc.stocks.vec  Vector of stock allocation fractions
#' @param ...              All other params passed to run_simulation()
#' @param progress_callback  Optional function(alloc_i, total_allocs, sim_i, total_sims, msg)
#' @return Data frame with columns: allocation, rebal_gain, rebal_sd,
#'         nonrebal_gain, nonrebal_sd, s.int, b.int
run_meta_sweep <- function(perc.stocks.vec, n, s.mean, s.sd, b.mean, b.sd,
                           s.b.corr, init.inv, rebal.interval,
                           s.int, b.int, int.period,
                           propStepDivisor, nSims, nSimsToRecord,
                           max_cloud_sims = Inf,
                           progress_callback = NULL) {

  summary_results <- list()
  cloud_results <- list()
  point_est_results <- list()
  sim_history_results <- list()

  for (k in seq_along(perc.stocks.vec)) {
    alloc <- perc.stocks.vec[k]

    # Build a sim-level callback that also reports allocation progress
    sim_cb <- NULL
    if (!is.null(progress_callback)) {
      sim_cb <- function(i, total, msg) {
        progress_callback(k, length(perc.stocks.vec), i, total,
                          paste0("Allocation ", k, "/", length(perc.stocks.vec),
                                 " (", round(alloc * 100), "% stocks): sim ",
                                 i, "/", total))
      }
    }

    res <- run_simulation(
      n = n, s.mean = s.mean, s.sd = s.sd, b.mean = b.mean, b.sd = b.sd,
      s.b.corr = s.b.corr, init.inv = init.inv, perc.stocks = alloc,
      rebal.interval = rebal.interval, s.int = s.int, b.int = b.int,
      int.period = int.period, propStepDivisor = propStepDivisor,
      nSims = nSims, nSimsToRecord = nSimsToRecord,
      progress_callback = sim_cb
    )

    # Capture subsampled price paths for first 10 sims
    n_record <- min(10, nSims)
    n_steps <- res$params$n
    subsample_times <- c(1L, seq(5L, n_steps, by = 5L))
    hist_rows <- list()
    for (s in seq_len(n_record)) {
      sim_costs <- res$costsDf[res$costsDf$sim == s & res$costsDf$time %in% subsample_times, ]
      stocks <- sim_costs$cost[sim_costs$class == "stocks"]
      bonds  <- sim_costs$cost[sim_costs$class == "bonds"]
      hist_rows[[s]] <- data.frame(
        allocation = alloc, sim = s, time = subsample_times,
        stock_price = stocks, bond_price = bonds
      )
    }
    sim_history_results[[k]] <- do.call(rbind, hist_rows)

    # Summary row (existing frontier data)
    pe <- res$gainsVsSD.point.estDf
    summary_results[[k]] <- data.frame(
      allocation = alloc,
      rebal_gain = pe$gain[pe$type == "rebal"],
      rebal_sd = pe$sd[pe$type == "rebal"],
      nonrebal_gain = pe$gain[pe$type == "nonrebal"],
      nonrebal_sd = pe$sd[pe$type == "nonrebal"],
      s.int = s.int,
      b.int = b.int
    )

    # Per-allocation cloud data (individual sim results, sampled if needed)
    cloud_df <- res$gainVsSDDf
    if (max_cloud_sims > 0 && max(cloud_df$sim) > max_cloud_sims) {
      keep_sims <- sample(unique(cloud_df$sim), max_cloud_sims)
      cloud_df <- cloud_df[cloud_df$sim %in% keep_sims, ]
    }
    cloud_df$allocation <- alloc
    cloud_results[[k]] <- cloud_df

    # Per-allocation point estimates
    pe$allocation <- alloc
    point_est_results[[k]] <- pe
  }

  list(
    summary = do.call(rbind, summary_results),
    cloud = do.call(rbind, cloud_results),
    point_estimates = do.call(rbind, point_est_results),
    sim_histories = do.call(rbind, sim_history_results)
  )
}
