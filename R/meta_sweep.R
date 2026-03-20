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
                           widthVal, progress_callback = NULL) {

  results <- list()

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
      widthVal = widthVal, progress_callback = sim_cb
    )

    pe <- res$gainsVsSD.point.estDf
    results[[k]] <- data.frame(
      allocation = alloc,
      rebal_gain = pe$gain[pe$type == "rebal"],
      rebal_sd = pe$sd[pe$type == "rebal"],
      nonrebal_gain = pe$gain[pe$type == "nonrebal"],
      nonrebal_sd = pe$sd[pe$type == "nonrebal"],
      s.int = s.int,
      b.int = b.int
    )
  }

  do.call(rbind, results)
}
