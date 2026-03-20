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
