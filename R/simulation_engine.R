# simulation_engine.R — Core MCMC portfolio simulation

#' Build the 2x2 stock-bond covariance matrix
#' @param s.sd Stock cost standard deviation
#' @param b.sd Bond cost standard deviation
#' @param s.b.corr Stock-bond correlation
#' @param propStepDivisor Proposal step divisor
#' @return 2x2 covariance matrix
build_covariance_matrix <- function(s.sd, b.sd, s.b.corr, propStepDivisor) {
  sdVec <- c(s.sd, b.sd) / propStepDivisor
  sdM <- diag(sdVec)
  rho <- matrix(c(1, rep(s.b.corr, 2), 1), ncol = 2)
  sdM %*% rho %*% sdM
}

#' Build increasing mean interest vectors
#' @param n Number of timesteps
#' @param s.mean Stock cost mean
#' @param b.mean Bond cost mean
#' @param s.int Stock interest rate
#' @param b.int Bond interest rate
#' @param int.period Interest compounding period
#' @return List with s.mean.vec, b.mean.vec, mean.vecDf
build_interest_vectors <- function(n, s.mean, b.mean, s.int, b.int, int.period) {
  s.int.vec <- sapply(1:n, function(x) (1 + s.int)^((x - 1) / int.period)) - 1
  b.int.vec <- sapply(1:n, function(x) (1 + b.int)^((x - 1) / int.period)) - 1

  s.mean.vec <- s.mean * (1 + s.int.vec)
  b.mean.vec <- b.mean * (1 + b.int.vec)

  mean.vecDf <- data.frame(
    time = rep(1:n, 2),
    class = rep(c("stocks", "bonds"), each = n),
    val = c(stocks = s.mean.vec, bonds = b.mean.vec)
  )

  list(s.mean.vec = s.mean.vec, b.mean.vec = b.mean.vec, mean.vecDf = mean.vecDf)
}

#' Run the core portfolio simulation loop
#' @param n Number of timesteps
#' @param nSims Total number of simulations
#' @param nSimsToRecord Number of sims to record full trajectories
#' @param s.mean Stock cost mean
#' @param b.mean Bond cost mean
#' @param s.sd Stock cost standard deviation
#' @param b.sd Bond cost standard deviation
#' @param s.mean.vec Stock mean vector with interest
#' @param b.mean.vec Bond mean vector with interest
#' @param init.inv Initial investment
#' @param perc.stocks Fraction allocated to stocks (0-1)
#' @param rebal.interval Rebalance every N timesteps
#' @param s.b.cov 2x2 covariance matrix
#' @param progress_callback Optional function(i, total, msg)
#' @return List of raw arrays: costs, value.rebal.tot, value.nonrebal.tot,
#'   value.rebal, value.nonrebal, shares.rebal, shares.nonrebal
simulate_portfolios <- function(n, nSims, nSimsToRecord, s.mean, b.mean, s.sd, b.sd,
                                s.mean.vec, b.mean.vec, init.inv, perc.stocks,
                                rebal.interval, s.b.cov, progress_callback = NULL) {
  # Initialize arrays
  shares.rebal <- array(dim = c(n, 2, nSimsToRecord))
  shares.nonrebal <- array(dim = c(n, 2, nSimsToRecord))
  value.rebal <- array(dim = c(n, 2, nSimsToRecord))
  value.nonrebal <- array(dim = c(n, 2, nSimsToRecord))

  shares.rebal.tmp <- array(dim = c(n, 2))
  shares.nonrebal.tmp <- array(dim = c(n, 2))
  value.rebal.tmp <- array(dim = c(n, 2))
  value.nonrebal.tmp <- array(dim = c(n, 2))

  shares.rebal.final <- array(dim = c(2, nSims))
  shares.nonrebal.final <- array(dim = c(2, nSims))
  value.rebal.final <- array(dim = c(2, nSims))
  value.nonrebal.final <- array(dim = c(2, nSims))

  value.rebal.tot <- array(dim = c(n, nSims))
  value.nonrebal.tot <- array(dim = c(n, nSims))
  costs <- array(dim = c(n, 2, nSims))

  for (j in 1:nSims) {

    if (!is.null(progress_callback)) {
      progress_callback(j, nSims, paste("Running simulation", j, "of", nSims))
    }

    # Random starting costs
    costs[1, 1, j] <- rtruncnorm(1, a = 0.5, mean = s.mean, sd = s.sd)
    costs[1, 2, j] <- rtruncnorm(1, a = 0.5, mean = b.mean, sd = b.sd)

    # Generate cost time series via Metropolis sampling
    for (i in 2:n) {
      p.s.prev <- dnorm(x = costs[i - 1, 1, j], mean = s.mean, sd = s.sd)
      p.b.prev <- dnorm(x = costs[i - 1, 2, j], mean = b.mean, sd = b.sd)

      new <- rmvnorm(n = 1, mean = costs[i - 1, , j], sigma = s.b.cov)

      p.s.new <- dnorm(x = new[1], mean = s.mean, sd = s.sd)
      p.b.new <- dnorm(x = new[2], mean = b.mean, sd = b.sd)

      eps <- 1e-4
      costs[i, 1, j] <- ifelse(runif(1) < p.s.new / p.s.prev & new[1] > eps,
                                new[1], costs[i - 1, 1, j])
      costs[i, 2, j] <- ifelse(runif(1) < p.b.new / p.b.prev & new[2] > eps,
                                new[2], costs[i - 1, 2, j])
    }

    # Add interest appreciation
    costs[, 1, j] <- costs[, 1, j] + (s.mean.vec - s.mean)
    costs[, 2, j] <- costs[, 2, j] + (b.mean.vec - b.mean)

    # Compute holdings for rebalanced portfolio
    perc.alloc.target <- c(perc.stocks, 1 - perc.stocks)

    shares.rebal.tmp[1, 1] <- (init.inv * perc.stocks) / costs[1, 1, j]
    shares.rebal.tmp[1, 2] <- (init.inv * (1 - perc.stocks)) / costs[1, 2, j]
    value.rebal.tmp[1, ] <- shares.rebal.tmp[1, ] * costs[1, , j]
    value.rebal.tot[1, j] <- sum(value.rebal.tmp[1, ])

    for (i in 2:n) {
      shares.rebal.tmp[i, ] <- shares.rebal.tmp[i - 1, ]
      value.rebal.tmp[i, ] <- shares.rebal.tmp[i, ] * costs[i, , j]
      value.rebal.tot[i, j] <- sum(value.rebal.tmp[i, ])

      if (i %% rebal.interval == 0) {
        holdings.perc <- value.rebal.tmp[i, ] / sum(value.rebal.tmp[i, ])
        del.perc <- holdings.perc - perc.alloc.target
        del.max <- del.perc > 0
        gain <- del.perc[del.max] * sum(value.rebal.tmp[i, ])

        shares.rebal.tmp[i, del.max] <- perc.alloc.target[del.max] *
          value.rebal.tot[i, j] / costs[i, del.max, j]
        shares.rebal.tmp[i, !del.max] <- shares.rebal.tmp[i, !del.max] +
          gain / costs[i, !del.max, j]

        value.rebal.tmp[i, ] <- shares.rebal.tmp[i, ] * costs[i, , j]
        value.rebal.tot[i, j] <- sum(value.rebal.tmp[i, ])
      }
    }

    # Non-rebalanced portfolio
    shares.nonrebal.tmp[1, ] <- shares.rebal.tmp[1, ]
    value.nonrebal.tmp[1, ] <- value.rebal.tmp[1, ]
    value.nonrebal.tot[1, j] <- value.rebal.tot[1, j]

    for (i in 2:n) {
      shares.nonrebal.tmp[i, ] <- shares.nonrebal.tmp[i - 1, ]
      value.nonrebal.tmp[i, ] <- shares.nonrebal.tmp[i, ] * costs[i, , j]
      value.nonrebal.tot[i, j] <- sum(value.nonrebal.tmp[i, ])
    }

    # Record full trajectories for nSimsToRecord sims
    if (j <= nSimsToRecord) {
      shares.rebal[, , j] <- shares.rebal.tmp
      shares.nonrebal[, , j] <- shares.nonrebal.tmp
      value.rebal[, , j] <- value.rebal.tmp
      value.nonrebal[, , j] <- value.nonrebal.tmp
    }

    # Record final timestep
    shares.rebal.final[, j] <- shares.rebal.tmp[n, ]
    shares.nonrebal.final[, j] <- shares.nonrebal.tmp[n, ]
    value.rebal.final[, j] <- value.rebal.tmp[n, ]
    value.nonrebal.final[, j] <- value.nonrebal.tmp[n, ]
  }

  list(
    costs = costs,
    value.rebal.tot = value.rebal.tot,
    value.nonrebal.tot = value.nonrebal.tot,
    value.rebal = value.rebal,
    value.nonrebal = value.nonrebal,
    shares.rebal = shares.rebal,
    shares.nonrebal = shares.nonrebal
  )
}

#' Convert raw simulation arrays into tidy data frames
#' @param n Number of timesteps
#' @param nSims Total number of simulations
#' @param nSimsToRecord Number of recorded sims
#' @param arrays List of raw arrays from simulate_portfolios()
#' @return List with costsDf, valueDf, sharesDf, value.totDf, deltaDf
build_result_dataframes <- function(n, nSims, nSimsToRecord, arrays) {
  value.rebal.tot <- arrays$value.rebal.tot
  value.nonrebal.tot <- arrays$value.nonrebal.tot

  # Total value trajectories
  value.rebal.totDf <- data.frame(
    time = rep(1:n, nSims),
    sim = rep(1:nSims, each = n),
    type = rep("total rebal", n * nSims),
    val = as.vector(value.rebal.tot)
  )
  value.nonrebal.totDf <- data.frame(
    time = rep(1:n, nSims),
    sim = rep(1:nSims, each = n),
    type = rep("total non-rebal", n * nSims),
    val = as.vector(value.nonrebal.tot)
  )
  value.totDf <- rbind(value.rebal.totDf, value.nonrebal.totDf)

  # Per-asset value trajectories
  value.rebalDf <- data.frame(
    time = rep(1:n, nSimsToRecord * 2),
    class = rep(rep(c("stocks", "bonds"), each = n), nSimsToRecord),
    sim = rep(1:nSimsToRecord, each = n * 2),
    type = rep("rebal", n * nSimsToRecord * 2),
    val = as.vector(arrays$value.rebal)
  )
  value.nonrebalDf <- data.frame(
    time = rep(1:n, nSimsToRecord * 2),
    class = rep(rep(c("stocks", "bonds"), each = n), nSimsToRecord),
    sim = rep(1:nSimsToRecord, each = n * 2),
    type = rep("nonrebal", n * nSimsToRecord * 2),
    val = as.vector(arrays$value.nonrebal)
  )
  valueDf <- rbind(value.rebalDf, value.nonrebalDf)

  # Shares
  shares.rebalDf <- data.frame(
    time = rep(1:n, nSimsToRecord * 2),
    class = rep(rep(c("stocks", "bonds"), each = n), nSimsToRecord),
    sim = rep(1:nSimsToRecord, each = n * 2),
    type = rep("rebal", n * nSimsToRecord * 2),
    shares = as.vector(arrays$shares.rebal)
  )
  shares.nonrebalDf <- data.frame(
    time = rep(1:n, nSimsToRecord * 2),
    class = rep(rep(c("stocks", "bonds"), each = n), nSimsToRecord),
    sim = rep(1:nSimsToRecord, each = n * 2),
    type = rep("nonrebal", n * nSimsToRecord * 2),
    shares = as.vector(arrays$shares.nonrebal)
  )
  sharesDf <- rbind(shares.rebalDf, shares.nonrebalDf)

  # Costs
  costsDf <- data.frame(
    time = rep(1:n, nSims * 2),
    class = rep(rep(c("stocks", "bonds"), each = n), nSims),
    sim = rep(1:nSims, each = n * 2),
    cost = as.vector(arrays$costs)
  )

  # Delta at final time
  deltaDf <- data.frame(
    rebal = value.rebal.totDf[value.rebal.totDf$time == n, ]$val,
    nonrebal = value.nonrebal.totDf[value.nonrebal.totDf$time == n, ]$val
  )
  deltaDf$delta <- deltaDf$rebal - deltaDf$nonrebal

  list(
    costsDf = costsDf,
    valueDf = valueDf,
    sharesDf = sharesDf,
    value.totDf = value.totDf,
    deltaDf = deltaDf
  )
}

#' Compute rolling standard deviation for rebalanced and non-rebalanced portfolios
#' @param value.rebal.tot Matrix of rebalanced total values (n x nSims)
#' @param value.nonrebal.tot Matrix of non-rebalanced total values (n x nSims)
#' @param n Number of timesteps
#' @param nSims Total number of simulations
#' @param widthVal Rolling window width
#' @return List with errorSD.rebal and errorSD.nonrebal vectors
compute_rolling_sd <- function(value.rebal.tot, value.nonrebal.tot, n, nSims, widthVal) {
  mean.rebal.mat <- matrix(nrow = n - widthVal + 1, ncol = nSims)
  error.rebal.mat <- matrix(nrow = n - widthVal + 1, ncol = nSims)
  errorSD.rebal <- vector(length = nSims)
  for (j in 1:nSims) {
    mean.rebal.mat[, j] <- rollapply(value.rebal.tot[, j], width = widthVal,
                                     align = "center", FUN = mean)
    error.rebal.mat[, j] <- value.rebal.tot[(widthVal / 2):(n - widthVal / 2), j] -
      mean.rebal.mat[, j]
    errorSD.rebal[j] <- sd(error.rebal.mat[, j])
  }

  mean.nonrebal.mat <- matrix(nrow = n - widthVal + 1, ncol = nSims)
  error.nonrebal.mat <- matrix(nrow = n - widthVal + 1, ncol = nSims)
  errorSD.nonrebal <- vector(length = nSims)
  for (j in 1:nSims) {
    mean.nonrebal.mat[, j] <- rollapply(value.nonrebal.tot[, j], width = widthVal,
                                        align = "center", FUN = mean)
    error.nonrebal.mat[, j] <- value.nonrebal.tot[(widthVal / 2):(n - widthVal / 2), j] -
      mean.nonrebal.mat[, j]
    errorSD.nonrebal[j] <- sd(error.nonrebal.mat[, j])
  }

  list(errorSD.rebal = errorSD.rebal, errorSD.nonrebal = errorSD.nonrebal)
}

#' Compute gain-vs-SD summary data frames
#' @param value.rebal.tot Matrix of rebalanced total values (n x nSims)
#' @param value.nonrebal.tot Matrix of non-rebalanced total values (n x nSims)
#' @param errorSD.rebal Rebalanced rolling SD vector
#' @param errorSD.nonrebal Non-rebalanced rolling SD vector
#' @param init.inv Initial investment
#' @param n Number of timesteps
#' @param nSims Total number of simulations
#' @param int.period Interest compounding period
#' @return List with gainVsSDDf and gainsVsSD.point.estDf
compute_gain_summary <- function(value.rebal.tot, value.nonrebal.tot,
                                 errorSD.rebal, errorSD.nonrebal,
                                 init.inv, n, nSims, int.period) {
  gain.rebal <- value.rebal.tot[n, ] - init.inv
  gain.rebal <- annualizeFn(init.inv, gain.rebal, n, int.period)

  gain.nonrebal <- value.nonrebal.tot[n, ] - init.inv
  gain.nonrebal <- annualizeFn(init.inv, gain.nonrebal, n, int.period)

  gainVsSD.rebalDf <- data.frame(
    gain = gain.rebal, sd = errorSD.rebal,
    sim = 1:nSims, type = rep("rebal", nSims)
  )
  gainVsSD.nonrebalDf <- data.frame(
    gain = gain.nonrebal, sd = errorSD.nonrebal,
    sim = 1:nSims, type = rep("nonrebal", nSims)
  )
  gainVsSDDf <- rbind(gainVsSD.rebalDf, gainVsSD.nonrebalDf)

  gainsVsSD.point.estDf <- data.frame(
    sd = c(mean(errorSD.rebal), mean(errorSD.nonrebal)),
    gain = c(mean(gain.rebal), mean(gain.nonrebal)),
    type = c("rebal", "nonrebal")
  )

  list(gainVsSDDf = gainVsSDDf, gainsVsSD.point.estDf = gainsVsSD.point.estDf)
}

#' Run a single portfolio simulation across multiple sims
#'
#' Coordinator function that calls sub-functions in sequence.
#' Signature and return value are identical to the original.
#'
#' @param n             Number of timesteps
#' @param s.mean        Stock cost mean
#' @param s.sd          Stock cost standard deviation
#' @param b.mean        Bond cost mean
#' @param b.sd          Bond cost standard deviation
#' @param s.b.corr      Stock-bond correlation
#' @param init.inv      Initial investment ($)
#' @param perc.stocks   Fraction allocated to stocks (0-1)
#' @param rebal.interval Rebalance every N timesteps
#' @param s.int         Stock interest rate
#' @param b.int         Bond interest rate
#' @param int.period    Interest compounding period
#' @param propStepDivisor  Proposal step divisor (larger = slower exploration)
#' @param nSims         Total number of simulations
#' @param nSimsToRecord Number of sims to record full trajectories for
#' @param widthVal      Rolling window width for SD calculations
#' @param progress_callback  Optional function(i, total, msg) for progress
#' @return A named list with data frames and echoed params
run_simulation <- function(n, s.mean, s.sd, b.mean, b.sd, s.b.corr,
                           init.inv, perc.stocks, rebal.interval,
                           s.int, b.int, int.period,
                           propStepDivisor, nSims, nSimsToRecord,
                           widthVal, progress_callback = NULL) {

  # Avoid rebalance on the last timestep
  n <- n - 1

  # Step 1: Covariance matrix
  s.b.cov <- build_covariance_matrix(s.sd, b.sd, s.b.corr, propStepDivisor)

  # Step 2: Interest vectors
  interest <- build_interest_vectors(n, s.mean, b.mean, s.int, b.int, int.period)

  # Step 3: Core simulation loop
  arrays <- simulate_portfolios(
    n = n, nSims = nSims, nSimsToRecord = nSimsToRecord,
    s.mean = s.mean, b.mean = b.mean, s.sd = s.sd, b.sd = b.sd,
    s.mean.vec = interest$s.mean.vec, b.mean.vec = interest$b.mean.vec,
    init.inv = init.inv, perc.stocks = perc.stocks,
    rebal.interval = rebal.interval, s.b.cov = s.b.cov,
    progress_callback = progress_callback
  )

  # Step 4: Build tidy data frames
  dfs <- build_result_dataframes(n, nSims, nSimsToRecord, arrays)

  # Step 5: Rolling SD
  rolling <- compute_rolling_sd(
    arrays$value.rebal.tot, arrays$value.nonrebal.tot,
    n, nSims, widthVal
  )

  # Step 6: Gain summary
  gains <- compute_gain_summary(
    arrays$value.rebal.tot, arrays$value.nonrebal.tot,
    rolling$errorSD.rebal, rolling$errorSD.nonrebal,
    init.inv, n, nSims, int.period
  )

  # Return structured result (identical shape to original)
  list(
    costsDf = dfs$costsDf,
    valueDf = dfs$valueDf,
    sharesDf = dfs$sharesDf,
    value.totDf = dfs$value.totDf,
    gainVsSDDf = gains$gainVsSDDf,
    gainsVsSD.point.estDf = gains$gainsVsSD.point.estDf,
    deltaDf = dfs$deltaDf,
    mean.vecDf = interest$mean.vecDf,
    params = list(
      n = n, s.mean = s.mean, s.sd = s.sd, b.mean = b.mean, b.sd = b.sd,
      s.b.corr = s.b.corr, init.inv = init.inv, perc.stocks = perc.stocks,
      rebal.interval = rebal.interval, s.int = s.int, b.int = b.int,
      int.period = int.period, propStepDivisor = propStepDivisor,
      nSims = nSims, nSimsToRecord = nSimsToRecord, widthVal = widthVal
    )
  )
}
