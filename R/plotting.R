# plotting.R — Plot functions, each returning a ggplot object

#' Plot 1: Gain vs SD scatterplot (rebal & non-rebal)
#' @param highlight_sim If set to a sim number, highlights that sim's points
#'   with a larger outlined marker (useful for screenshots). Set to NULL to disable.
#' @param show_cloud If FALSE, hides the sim cloud and shows only point estimates
#'   (useful for "point estimate only" screenshot). Default TRUE.
plot_gain_vs_sd <- function(result, highlight_sim = NULL, show_cloud = TRUE) {
  p <- result$params
  cloud_alpha <- if (show_cloud) app_theme$cloud_alpha else 0
  plt <- ggplot(data = result$gainVsSDDf, aes(x = sd, y = gain, color = type)) +
    geom_point(alpha = cloud_alpha) +
    geom_point(data = result$gainsVsSD.point.estDf,
               aes(x = sd, y = gain, color = type),
               size = app_theme$point_size, alpha = app_theme$point_alpha) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "standard deviation", y = "gain (annualized %)", color = NULL) +
    ggtitle(paste0("Stocks / Bonds = ", round(p$perc.stocks * 100, 1), "% / ",
                   round((1 - p$perc.stocks) * 100, 1), "%")) +
    app_theme$base_theme()

  if (!is.null(highlight_sim)) {
    hl <- result$gainVsSDDf[result$gainVsSDDf$sim == highlight_sim, ]
    plt <- plt +
      geom_point(data = hl, aes(x = sd, y = gain),
                 size = 5, shape = 21, fill = "yellow", color = "black",
                 stroke = 1.2, alpha = 0.9)
  }
  plt
}

#' Plot 6: Total value trajectories for multiple sims
plot_trajectories <- function(result, max_sims = 100) {
  df <- result$value.totDf[result$value.totDf$sim <= max_sims, ]
  ggplot(data = df,
         aes(x = time, y = val,
             group = interaction(type, sim), color = type)) +
    geom_line(alpha = app_theme$traj_alpha) +
    ylab("portfolio value ($)") +
    xlab("day") +
    ggtitle(paste0("Portfolio Trajectories (up to ", max_sims, " sims)")) +
    app_theme$base_theme()
}

#' Plot 7: Cost (price) profiles
plot_cost_profiles <- function(result, max_sims = 10) {
  df <- result$costsDf[result$costsDf$sim <= max_sims, ]
  ggplot(data = df,
         aes(x = time, y = cost,
             group = interaction(class, sim), color = class)) +
    geom_line(alpha = app_theme$cost_alpha) +
    ylab("cost per share ($)") +
    xlab("day") +
    ggtitle(paste0("Share Cost Profiles (up to ", max_sims, " sims)")) +
    app_theme$base_theme()
}

#' Plot 8: Final value densities
plot_final_densities <- function(result) {
  n <- result$params$n
  dfFinal <- result$value.totDf[result$value.totDf$time == n, ]
  ggplot(data = dfFinal, aes(x = val, group = type, color = type, fill = type)) +
    geom_density(alpha = app_theme$density_alpha) +
    xlab("final portfolio value ($)") +
    ggtitle("Distribution of Final Portfolio Values") +
    app_theme$base_theme()
}

#' Plot 9: Density of rebal vs non-rebal delta
plot_rebal_delta <- function(result) {
  medianDel <- median(result$deltaDf$delta)
  ggplot(data = result$deltaDf, aes(x = delta)) +
    geom_density(color = app_theme$delta_color, fill = app_theme$delta_color,
                 alpha = app_theme$delta_alpha) +
    geom_vline(xintercept = medianDel, color = app_theme$delta_color,
               linetype = "dashed") +
    ggtitle(paste("Rebal vs Non-Rebal Delta; median =",
                  round(medianDel, 2))) +
    xlab("delta ($)") +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10))
}

#' Plot 10: Single sim explorer (multi-panel with plotly::subplot)
plot_single_sim_explorer <- function(result, simSel) {
  valueDf <- result$valueDf
  value.totDf <- result$value.totDf
  sharesDf <- result$sharesDf
  costsDf <- result$costsDf
  mean.vecDf <- result$mean.vecDf
  nSimsToRecord <- result$params$nSimsToRecord

  if (simSel > nSimsToRecord) {
    p <- ggplot() +
      annotate("text", x = 0.5, y = 0.5,
               label = paste("Only", nSimsToRecord,
                             "sims have full trajectory data.\nSelect sim <=",
                             nSimsToRecord),
               size = 5) +
      theme_void()
    return(ggplotly(p))
  }

  # Panel 1: Rebalanced asset values + non-rebal totals
  value.sim.selDf <- valueDf[valueDf$sim == simSel & valueDf$type == "rebal", ]
  value.tot.sim.selDf <- value.totDf[value.totDf$sim == simSel, ]
  plt.vals <- ggplot() +
    geom_line(data = value.sim.selDf, aes(x = time, y = val, color = class)) +
    geom_line(data = value.tot.sim.selDf, aes(x = time, y = val, color = type)) +
    labs(y = "value ($)", color = NULL) +
    ggtitle(paste("Rebalanced Asset Values (& Non-rebal Totals); Sim #", simSel)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 2: Shares held (rebalanced)
  shares.sim.selDf <- sharesDf[sharesDf$sim == simSel & sharesDf$type == "rebal", ]
  plt.shares <- ggplot(data = shares.sim.selDf,
                       aes(x = time, y = shares, color = class)) +
    geom_line() +
    labs(color = NULL) +
    ggtitle(paste("Shares Held, Rebalanced; Sim #", simSel)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 3: Costs per share
  costs.sim.selDf <- costsDf[costsDf$sim == simSel, ]
  plt.costs <- ggplot(data = costs.sim.selDf,
                      aes(x = time, y = cost, color = class)) +
    geom_line() +
    geom_line(data = mean.vecDf, aes(x = time, y = val, color = class),
              alpha = app_theme$mean_line_alpha, linetype = "dashed") +
    labs(color = NULL) +
    ggtitle(paste("Costs per Share; Sim #", simSel)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10))

  # Convert each to plotly and stack vertically
  p1 <- ggplotly(plt.vals) %>% layout(showlegend = TRUE)
  p2 <- ggplotly(plt.shares) %>% layout(showlegend = FALSE)
  p3 <- ggplotly(plt.costs) %>% layout(showlegend = FALSE)

  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, heights = c(0.5, 0.25, 0.25),
          titleY = TRUE) %>%
    layout(
      title = list(text = paste("Single Sim Explorer — Sim #", simSel),
                   font = list(size = 14, color = app_theme$text_color)),
      paper_bgcolor = app_theme$panel_bg,
      plot_bgcolor = app_theme$panel_bg
    )
}

#' Frontier explorer: cloud of sim results per allocation with frontier overlay
plot_frontier_explorer <- function(sweep_data, alloc_index) {
  allocs <- sort(unique(sweep_data$point_estimates$allocation))
  current_alloc <- allocs[alloc_index]

  all_pe <- sweep_data$point_estimates
  current_cloud <- sweep_data$cloud[sweep_data$cloud$allocation == current_alloc, ]
  current_pe <- all_pe[all_pe$allocation == current_alloc, ]
  other_pe <- all_pe[all_pe$allocation != current_alloc, ]

  # Consistent axis limits across all allocations for comparability
  all_cloud <- sweep_data$cloud
  cloud_sd_q <- quantile(all_cloud$sd, c(0.02, 0.98), na.rm = TRUE)
  cloud_gain_q <- quantile(all_cloud$gain, c(0.02, 0.98), na.rm = TRUE)
  pe_sd_range <- range(all_pe$sd)
  pe_gain_range <- range(all_pe$gain)

  x_lim <- c(min(pe_sd_range[1], cloud_sd_q[1]), max(pe_sd_range[2], cloud_sd_q[2]))
  y_lim <- c(min(pe_gain_range[1], cloud_gain_q[1]), max(pe_gain_range[2], cloud_gain_q[2]))
  x_pad <- diff(x_lim) * 0.05
  y_pad <- diff(y_lim) * 0.05
  x_lim <- x_lim + c(-x_pad, x_pad)
  y_lim <- y_lim + c(-y_pad, y_pad)

  # Allocation labels on rebal frontier points only (avoid clutter)
  rebal_pe <- all_pe[all_pe$type == "rebal", ]

  # Use a single color aesthetic throughout so ggplotly merges legends cleanly
  ggplot() +
    # Cloud of individual sim results for current allocation
    geom_point(data = current_cloud, aes(x = sd, y = gain, color = type),
               alpha = app_theme$cloud_alpha) +
    # Frontier lines connecting point estimates by type
    geom_line(data = all_pe[all_pe$type == "rebal", ],
              aes(x = sd, y = gain),
              color = app_theme$frontier_line_color,
              alpha = app_theme$frontier_line_alpha) +
    geom_line(data = all_pe[all_pe$type == "nonrebal", ],
              aes(x = sd, y = gain),
              color = app_theme$frontier_line_color,
              alpha = app_theme$frontier_line_alpha) +
    # Other allocation point estimates
    geom_point(data = other_pe, aes(x = sd, y = gain, color = type),
               size = 2, alpha = app_theme$other_point_alpha) +
    # Current allocation point estimates (highlighted)
    geom_point(data = current_pe, aes(x = sd, y = gain, color = type),
               size = app_theme$current_point_size,
               alpha = app_theme$current_point_alpha) +
    # Allocation % labels along the rebal frontier
    geom_text(data = rebal_pe,
              aes(x = sd, y = gain,
                  label = paste0(round(allocation * 100), "%")),
              size = app_theme$frontier_label_size, vjust = -1,
              color = app_theme$frontier_label_color) +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "standard deviation", y = "gain (annualized %)", color = NULL) +
    ggtitle(paste0("Frontier Explorer: ",
                   round(current_alloc * 100, 1), "% Stocks / ",
                   round((1 - current_alloc) * 100, 1), "% Bonds")) +
    app_theme$base_theme()
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

  ggplot(data = plot_df, aes(x = sd, y = gain, color = type)) +
    geom_point(size = 2) +
    geom_line(alpha = app_theme$mean_line_alpha) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "standard deviation", y = "gain (annualized %)", color = NULL) +
    ggtitle("Efficient Frontier: Gain vs Risk by Allocation") +
    app_theme$base_theme()
}
