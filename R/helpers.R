# helpers.R — Shared utility functions

#' Annualize a gain over n timesteps given an interest period
#' @param P  Principal (initial investment)
#' @param G  Total gain (final - initial)
#' @param n  Number of timesteps
#' @param int.period  Interest compounding period (in timesteps)
#' @return Annualized percentage return
annualizeFn <- function(P, G, n, int.period) {
  AP <- ((P + G) / P)^(1 / (n / int.period)) - 1
  return(AP)
}

#' Convert percentage(s) to fractions clamped to (0.000001, 0.999999)
#' @param pct Numeric vector of percentages (0-100 scale)
#' @return Numeric vector of fractions
clamp_pct_to_fraction <- function(pct) {
  pmax(0.000001, pmin(0.999999, pct / 100))
}

#' Parse a comma-separated allocation string into clamped fractions
#' @param alloc_string Character string of comma-separated percentages
#' @return Numeric vector of fractions
parse_allocation_string <- function(alloc_string) {
  alloc_pcts <- as.numeric(trimws(unlist(strsplit(alloc_string, ","))))
  alloc_pcts <- alloc_pcts[!is.na(alloc_pcts)]
  clamp_pct_to_fraction(alloc_pcts)
}

#' Format the gain-vs-SD point estimate data frame for display
#' @param df Data frame with columns: sd, gain, type
#' @return Formatted data frame with renamed columns
format_summary_table <- function(df) {
  df$gain <- paste0(round(df$gain * 100, 3), "%")
  df$sd <- paste0(round(df$sd * 100, 2), "%")
  names(df) <- c("Annualized Vol", "Annualized Gain", "Strategy")
  df
}

#' Format simulation parameters as a readable summary string
#' @param params Named list of simulation parameters
#' @return Single character string with formatted summary
format_params_summary <- function(params) {
  p <- params
  paste0(
    "Timesteps: ", p$n, "\n",
    "Stock Mean: ", p$s.mean, " | SD: ", p$s.sd, "\n",
    "Bond Mean: ", p$b.mean, " | SD: ", p$b.sd, "\n",
    "Correlation: ", p$s.b.corr, "\n",
    "Initial Investment: $ ", p$init.inv, "\n",
    "Stock Allocation: ", round(p$perc.stocks * 100, 1), " %\n",
    "Rebalance Interval: ", p$rebal.interval, " timesteps\n",
    "Stock Interest: ", p$s.int, " | Bond Interest: ", p$b.int, "\n",
    "Interest Period: ", p$int.period, " timesteps\n",
    "Simulations: ", p$nSims, " ( ", p$nSimsToRecord, " recorded)\n",
    "Proposal Step Divisor: ", p$propStepDivisor, "\n"
  )
}

#' Compute a credible/coverage band from a numeric vector
#' @param x Numeric vector of values
#' @param band_type "percentile" or "hdi"
#' @param coverage Coverage level (0-1), e.g. 0.80 for 80%
#' @return Named numeric vector with elements "lower" and "upper"
compute_band <- function(x, band_type = "percentile", coverage = 0.80) {
  x <- x[!is.na(x)]
  if (band_type == "percentile") {
    alpha <- (1 - coverage) / 2
    q <- quantile(x, probs = c(alpha, 1 - alpha))
    return(c(lower = unname(q[1]), upper = unname(q[2])))
  } else if (band_type == "hdi") {
    # Naive HDI: slide a window of size ceil(coverage * N) across sorted values,
    # find the window with the smallest range
    n <- length(x)
    x_sorted <- sort(x)
    window_size <- ceiling(coverage * n)
    if (window_size >= n) return(c(lower = x_sorted[1], upper = x_sorted[n]))
    n_windows <- n - window_size + 1L
    widths <- x_sorted[(window_size):n] - x_sorted[1:n_windows]
    best <- which.min(widths)
    return(c(lower = x_sorted[best], upper = x_sorted[best + window_size - 1L]))
  } else {
    stop("Unknown band_type: ", band_type, ". Use 'percentile' or 'hdi'.")
  }
}

#' Compute summary bands from frontier cloud data
#' @param cloud_df Data frame with columns: allocation, type, gain, sd, sim
#' @param band_type "percentile" or "hdi"
#' @param coverage Coverage level (0-1), default 0.80
#' @return Data frame with columns: allocation, type, mean_gain, median_gain,
#'   mean_sd, median_sd, lower_gain, upper_gain, lower_sd, upper_sd, alloc_pct
compute_frontier_bands <- function(cloud_df, band_type = "percentile",
                                   coverage = 0.80) {
  groups <- split(cloud_df, list(cloud_df$allocation, cloud_df$type),
                  drop = TRUE)

  rows <- lapply(groups, function(g) {
    gain_band <- compute_band(g$gain, band_type = band_type, coverage = coverage)
    sd_band   <- compute_band(g$sd,   band_type = band_type, coverage = coverage)
    data.frame(
      allocation   = g$allocation[1],
      type         = g$type[1],
      mean_gain    = mean(g$gain, na.rm = TRUE),
      median_gain  = median(g$gain, na.rm = TRUE),
      mean_sd      = mean(g$sd, na.rm = TRUE),
      median_sd    = median(g$sd, na.rm = TRUE),
      lower_gain   = gain_band[["lower"]],
      upper_gain   = gain_band[["upper"]],
      lower_sd     = sd_band[["lower"]],
      upper_sd     = sd_band[["upper"]],
      stringsAsFactors = FALSE
    )
  })

  bands <- do.call(rbind, rows)
  rownames(bands) <- NULL
  bands$alloc_pct <- round(bands$allocation * 100, 1)
  bands <- bands[order(bands$type, bands$alloc_pct), ]
  bands
}

#' Compute fixed y-axis ranges for the bands plot across both band types
#'
#' Computes percentile and HDI bands for all allocation/strategy groups, then
#' finds the global min/max across all four combinations (rebal-percentile,
#' rebal-HDI, nonrebal-percentile, nonrebal-HDI) with padding.
#' @param cloud_df Data frame with columns: allocation, type, gain, sd, sim
#' @param coverage Coverage level (0-1), default 0.80
#' @param pad Fractional padding above and below, default 0.05
#' @return List with elements: gain = c(lo, hi), sd = c(lo, hi)
compute_bands_ylim <- function(cloud_df, coverage = 0.80, pad = 0.05) {
  bands_pct <- compute_frontier_bands(cloud_df, band_type = "percentile",
                                       coverage = coverage)
  bands_hdi <- compute_frontier_bands(cloud_df, band_type = "hdi",
                                       coverage = coverage)
  all_bands <- rbind(bands_pct, bands_hdi)

  gain_lo <- min(all_bands$lower_gain, all_bands$mean_gain,
                 all_bands$median_gain, na.rm = TRUE)
  gain_hi <- max(all_bands$upper_gain, all_bands$mean_gain,
                 all_bands$median_gain, na.rm = TRUE)
  gain_pad <- (gain_hi - gain_lo) * pad

  sd_lo <- min(all_bands$lower_sd, all_bands$mean_sd,
               all_bands$median_sd, na.rm = TRUE)
  sd_hi <- max(all_bands$upper_sd, all_bands$mean_sd,
               all_bands$median_sd, na.rm = TRUE)
  sd_pad <- (sd_hi - sd_lo) * pad

  list(
    gain = c(gain_lo - gain_pad, gain_hi + gain_pad),
    sd   = c(sd_lo - sd_pad, sd_hi + sd_pad)
  )
}
