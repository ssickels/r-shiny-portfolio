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
