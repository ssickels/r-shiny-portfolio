# plotting.R — Plot functions, each returning a ggplot object

#' Plot 1: Gain vs SD scatterplot (rebal & non-rebal)
plot_gain_vs_sd <- function(result) {
  p <- result$params
  ggplot(data = result$gainVsSDDf, aes(x = sd, y = gain, color = type)) +
    geom_point(alpha = 0.08) +
    geom_point(data = result$gainsVsSD.point.estDf,
               aes(x = sd, y = gain, fill = type),
               size = 3, shape = 21, color = "black", alpha = 0.8) +
    geom_hline(yintercept = c(p$s.int, p$b.int),
               linetype = "dashed", color = "darkblue", alpha = 0.3) +
    ylab("gain (annualized %)") +
    xlab("standard deviation") +
    ggtitle(paste0("Stocks / Bonds = ", round(p$perc.stocks * 100, 1), "% / ",
                   round((1 - p$perc.stocks) * 100, 1), "%")) +
    scale_y_continuous(labels = scales::percent) +
    theme_light()
}

#' Plot 6: Total value trajectories for multiple sims
plot_trajectories <- function(result, max_sims = 100) {
  df <- result$value.totDf[result$value.totDf$sim <= max_sims, ]
  ggplot(data = df,
         aes(x = time, y = val,
             group = interaction(type, sim), color = type)) +
    geom_line(alpha = 0.1) +
    ylab("portfolio value ($)") +
    xlab("timestep") +
    ggtitle(paste0("Portfolio Trajectories (up to ", max_sims, " sims)")) +
    theme_light()
}

#' Plot 7: Cost (price) profiles
plot_cost_profiles <- function(result, max_sims = 10) {
  df <- result$costsDf[result$costsDf$sim <= max_sims, ]
  ggplot(data = df,
         aes(x = time, y = cost,
             group = interaction(class, sim), color = class)) +
    geom_line(alpha = 0.5) +
    ylab("cost per share ($)") +
    xlab("timestep") +
    ggtitle(paste0("Share Cost Profiles (up to ", max_sims, " sims)")) +
    theme_light()
}

#' Plot 8: Final value densities
plot_final_densities <- function(result) {
  n <- result$params$n
  dfFinal <- result$value.totDf[result$value.totDf$time == n, ]
  ggplot(data = dfFinal, aes(x = val, group = type, color = type, fill = type)) +
    geom_density(alpha = 0.2) +
    xlab("final portfolio value ($)") +
    ggtitle("Distribution of Final Portfolio Values") +
    theme_light()
}

#' Plot 9: Density of rebal vs non-rebal delta
plot_rebal_delta <- function(result) {
  medianDel <- median(result$deltaDf$delta)
  ggplot(data = result$deltaDf, aes(x = delta)) +
    geom_density(color = "blue", fill = "blue", alpha = 0.2) +
    geom_vline(xintercept = medianDel, color = "blue", linetype = "dashed") +
    ggtitle(paste("Rebal vs Non-Rebal Delta; median =",
                  round(medianDel, 2))) +
    xlab("delta ($)") +
    theme_light() +
    theme(plot.title = element_text(size = 10))
}

#' Plot 10: Single sim explorer (multi-panel with patchwork)
plot_single_sim_explorer <- function(result, simSel) {
  valueDf <- result$valueDf
  value.totDf <- result$value.totDf
  sharesDf <- result$sharesDf
  costsDf <- result$costsDf
  mean.vecDf <- result$mean.vecDf
  nSimsToRecord <- result$params$nSimsToRecord

  if (simSel > nSimsToRecord) {
    return(
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = paste("Only", nSimsToRecord,
                               "sims have full trajectory data.\nSelect sim <=",
                               nSimsToRecord),
                 size = 5) +
        theme_void()
    )
  }

  # Panel 1: Rebalanced asset values + non-rebal totals
  value.sim.selDf <- valueDf[valueDf$sim == simSel & valueDf$type == "rebal", ]
  value.tot.sim.selDf <- value.totDf[value.totDf$sim == simSel, ]
  plt.vals <- ggplot() +
    geom_line(data = value.sim.selDf, aes(x = time, y = val, color = class)) +
    geom_line(data = value.tot.sim.selDf, aes(x = time, y = val, color = type)) +
    ylab("value ($)") +
    ggtitle(paste("Rebalanced Asset Values (& Non-rebalanced Totals); Sim #", simSel)) +
    theme_light() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 2: Shares held (rebalanced)
  shares.sim.selDf <- sharesDf[sharesDf$sim == simSel & sharesDf$type == "rebal", ]
  plt.shares <- ggplot(data = shares.sim.selDf,
                       aes(x = time, y = shares, color = class)) +
    geom_line() +
    ggtitle(paste("Shares Held, Rebalanced; Sim #", simSel)) +
    theme_light() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 3: Costs per share
  costs.sim.selDf <- costsDf[costsDf$sim == simSel, ]
  plt.costs <- ggplot(data = costs.sim.selDf,
                      aes(x = time, y = cost, color = class)) +
    geom_line() +
    geom_line(data = mean.vecDf, aes(x = time, y = val, color = class),
              alpha = 0.5, linetype = "dashed") +
    ggtitle(paste("Costs per Share; Sim #", simSel)) +
    theme_light() +
    theme(plot.title = element_text(size = 10))

  plt.vals + plt.shares + plt.costs +
    plot_layout(ncol = 1, heights = c(3, 1, 1))
}

#' Efficient frontier plot from sweep results
plot_efficient_frontier <- function(sweep_result) {
  # Reshape to long format for plotting
  rebal_df <- data.frame(
    allocation = sweep_result$allocation,
    sd = sweep_result$rebal_sd,
    gain = sweep_result$rebal_gain,
    type = "rebal"
  )
  nonrebal_df <- data.frame(
    allocation = sweep_result$allocation,
    sd = sweep_result$nonrebal_sd,
    gain = sweep_result$nonrebal_gain,
    type = "nonrebal"
  )
  plot_df <- rbind(rebal_df, nonrebal_df)

  s.int <- sweep_result$s.int[1]
  b.int <- sweep_result$b.int[1]

  ggplot(data = plot_df, aes(x = sd, y = gain, color = type)) +
    geom_point(size = 2) +
    geom_line(alpha = 0.5) +
    geom_hline(yintercept = c(s.int, b.int),
               linetype = "dashed", color = "darkblue", alpha = 0.3) +
    scale_y_continuous(labels = scales::percent) +
    ylab("gain (annualized %)") +
    xlab("standard deviation") +
    ggtitle("Efficient Frontier: Gain vs Risk by Allocation") +
    theme_light()
}
