# plotting.R — Plot functions, each returning a ggplot object

#' Plot 1: Gain vs SD scatterplot (rebal & non-rebal)
#' @param highlight_sim If set to a sim number, highlights that sim's points
#'   with a larger outlined marker (useful for screenshots). Set to NULL to disable.
#' @param show_cloud If FALSE, hides the sim cloud and shows only point estimates
#'   (useful for "point estimate only" screenshot). Default TRUE.
plot_gain_vs_sd <- function(result, highlight_sim = NULL, show_cloud = TRUE,
                            cloud_alpha_override = NULL) {
  p <- result$params
  cloud_alpha <- if (!show_cloud) 0
                 else if (!is.null(cloud_alpha_override)) cloud_alpha_override
                 else app_theme$cloud_alpha
  plt <- ggplot(data = result$gainVsSDDf, aes(x = sd, y = gain, color = type)) +
    geom_point(alpha = cloud_alpha) +
    geom_point(data = result$gainsVsSD.point.estDf,
               aes(x = sd, y = gain, color = type),
               size = app_theme$point_size, alpha = app_theme$point_alpha) +
    scale_color_manual(values = app_theme$strategy_colors,
                       labels = app_theme$strategy_labels) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "volatility (annualized %)", y = "gain (annualized %)", color = NULL) +
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
    scale_color_manual(values = app_theme$explorer_colors) +
    labs(y = "value ($)", color = NULL) +
    ggtitle(paste("Rebalanced Asset Values (& Non-rebal Totals); Sim #", simSel)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 2: Shares held (rebalanced)
  shares.sim.selDf <- sharesDf[sharesDf$sim == simSel & sharesDf$type == "rebal", ]
  plt.shares <- ggplot(data = shares.sim.selDf,
                       aes(x = time, y = shares, color = class)) +
    geom_line() +
    scale_color_manual(values = app_theme$asset_colors) +
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
    scale_color_manual(values = app_theme$asset_colors) +
    labs(color = NULL) +
    ggtitle(paste("Costs per Share; Sim #", simSel)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10))

  # Convert each to plotly and stack vertically
  # Show legend on p1 traces only; hide duplicates from p2/p3
  p1 <- ggplotly(plt.vals)
  p2 <- ggplotly(plt.shares)
  p3 <- ggplotly(plt.costs)

  for (i in seq_along(p2$x$data)) p2$x$data[[i]]$showlegend <- FALSE
  for (i in seq_along(p3$x$data)) p3$x$data[[i]]$showlegend <- FALSE

  subplot(p1, p2, p3, nrows = 3, shareX = TRUE, heights = c(0.5, 0.25, 0.25),
          titleY = TRUE) %>%
    layout(
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02,
                    font = list(color = app_theme$text_color)),
      margin = list(t = 40),
      paper_bgcolor = app_theme$panel_bg,
      plot_bgcolor = app_theme$panel_bg
    )
}

#' Frontier explorer: cloud of sim results per allocation with frontier overlay
plot_frontier_explorer <- function(sweep_data, alloc_index,
                                   cloud_alpha_override = NULL,
                                   show_labels = TRUE,
                                   highlight_sim = NULL,
                                   show_cloud_rebal = TRUE,
                                   show_cloud_nonrebal = TRUE,
                                   show_line_rebal = TRUE,
                                   show_line_nonrebal = TRUE,
                                   show_dots_rebal = TRUE,
                                   show_dots_nonrebal = TRUE,
                                   highlight_current = TRUE,
                                   show_dots_others = TRUE,
                                   dot_size = 2,
                                   line_width = 0.5,
                                   line_alpha = app_theme$frontier_line_alpha,
                                   cloud_sample_n = NULL) {
  allocs <- sort(unique(sweep_data$point_estimates$allocation))
  current_alloc <- allocs[alloc_index]

  all_pe <- sweep_data$point_estimates
  current_cloud <- sweep_data$cloud[sweep_data$cloud$allocation == current_alloc, ]
  # Interleave rebal/nonrebal rows so neither color systematically covers the other
  current_cloud <- current_cloud[order(current_cloud$sim), ]
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

  cloud_alpha <- if (!is.null(cloud_alpha_override)) cloud_alpha_override
                 else app_theme$cloud_alpha

  # Subsample cloud for visual clarity at higher opacity
  if (!is.null(cloud_sample_n)) {
    all_sims <- unique(current_cloud$sim)
    if (cloud_sample_n < length(all_sims)) {
      set.seed(42)
      keep_sims <- sample(all_sims, cloud_sample_n)
      current_cloud <- current_cloud[current_cloud$sim %in% keep_sims, ]
    }
  }

  # Split cloud by strategy for independent toggle control
  cloud_rebal_df    <- current_cloud[current_cloud$type == "rebal", ]
  cloud_nonrebal_df <- current_cloud[current_cloud$type == "nonrebal", ]

  # Filter OTHER allocation PE dots by strategy toggles
  other_pe_show <- other_pe[
    (other_pe$type == "rebal" & show_dots_rebal) |
    (other_pe$type == "nonrebal" & show_dots_nonrebal), ]

  plt <- ggplot()

  # 1. Cloud layers (fixed color, no legend — legend comes from PE dots)
  if (show_cloud_rebal) {
    plt <- plt +
      geom_point(data = cloud_rebal_df, aes(x = sd, y = gain),
                 color = app_theme$rebal_color, alpha = cloud_alpha,
                 show.legend = FALSE)
  }
  if (show_cloud_nonrebal) {
    plt <- plt +
      geom_point(data = cloud_nonrebal_df, aes(x = sd, y = gain),
                 color = app_theme$nonrebal_color, alpha = cloud_alpha,
                 show.legend = FALSE)
  }

  # 2. Frontier lines
  if (show_line_rebal) {
    plt <- plt +
      geom_path(data = all_pe[all_pe$type == "rebal", ],
                aes(x = sd, y = gain),
                color = app_theme$frontier_line_color,
                alpha = line_alpha,
                linewidth = line_width)
  }
  if (show_line_nonrebal) {
    plt <- plt +
      geom_path(data = all_pe[all_pe$type == "nonrebal", ],
                aes(x = sd, y = gain),
                color = app_theme$frontier_line_color,
                alpha = line_alpha,
                linewidth = line_width)
  }

  # 3. Other allocation PE dots (controlled by strategy toggles + others toggle)
  if (show_dots_others && nrow(other_pe_show) > 0) {
    plt <- plt +
      geom_point(data = other_pe_show, aes(x = sd, y = gain, color = type),
                 size = dot_size, alpha = app_theme$other_point_alpha)
  }

  # 4. Current allocation PE dot (strategy toggles filter type; highlight controls size)
  current_pe_show <- current_pe[
    (current_pe$type == "rebal" & show_dots_rebal) |
    (current_pe$type == "nonrebal" & show_dots_nonrebal), ]
  if (nrow(current_pe_show) > 0) {
    cur_size  <- if (highlight_current) dot_size + 2 else dot_size
    cur_alpha <- if (highlight_current) app_theme$current_point_alpha else app_theme$other_point_alpha
    plt <- plt +
      geom_point(data = current_pe_show, aes(x = sd, y = gain, color = type),
                 size = cur_size, alpha = cur_alpha)
  }

  # Scales and theme
  has_color_aes <- (show_dots_others && nrow(other_pe_show) > 0) || nrow(current_pe_show) > 0
  plt <- plt +
    coord_cartesian(xlim = x_lim, ylim = y_lim) +
    { if (has_color_aes) scale_color_manual(values = app_theme$strategy_colors,
                                           labels = app_theme$strategy_labels) } +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "volatility (annualized %)", y = "gain (annualized %)", color = NULL) +
    ggtitle(paste0("Frontier Explorer: ",
                   round(current_alloc * 100, 1), "% Stocks / ",
                   round((1 - current_alloc) * 100, 1), "% Bonds")) +
    app_theme$base_theme()

  # Highlight a specific simulation pair (rebal + nonrebal connected by a line)
  if (!is.null(highlight_sim)) {
    hl <- current_cloud[current_cloud$sim == highlight_sim, ]
    if (nrow(hl) >= 2) {
      hl_rebal <- hl[hl$type == "rebal", ][1, ]
      hl_nonrebal <- hl[hl$type == "nonrebal", ][1, ]
      seg_df <- data.frame(x = hl_nonrebal$sd, xend = hl_rebal$sd,
                           y = hl_nonrebal$gain, yend = hl_rebal$gain)
      plt <- plt +
        geom_segment(data = seg_df,
                     aes(x = x, y = y, xend = xend, yend = yend),
                     color = "gray40", linewidth = 0.5, alpha = 0.7,
                     show.legend = FALSE) +
        geom_point(data = hl_rebal, aes(x = sd, y = gain),
                   shape = 23, fill = "#00BFC4", color = "black",
                   size = 4, stroke = 0.8, show.legend = FALSE) +
        geom_point(data = hl_nonrebal, aes(x = sd, y = gain),
                   shape = 23, fill = "#F8766D", color = "black",
                   size = 4, stroke = 0.8, show.legend = FALSE) +
        geom_text(data = hl_rebal, aes(x = sd, y = gain),
                  label = paste0("sim #", highlight_sim),
                  size = 3, vjust = -1.5, color = "gray30",
                  show.legend = FALSE)
    }
  }

  if (show_labels) {
    plt <- plt +
      geom_text(data = rebal_pe,
                aes(x = sd, y = gain,
                    label = paste0(round(allocation * 100), "%")),
                size = app_theme$frontier_label_size, vjust = -1,
                color = app_theme$frontier_label_color)
  }

  plt
}

#' Efficient frontier plot from sweep results
plot_efficient_frontier <- function(sweep_result, show_labels = TRUE) {
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

  plt <- ggplot(data = plot_df, aes(x = sd, y = gain, color = type)) +
    geom_point(size = 2) +
    geom_path(alpha = app_theme$mean_line_alpha) +
    scale_color_manual(values = app_theme$strategy_colors,
                       labels = app_theme$strategy_labels) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "volatility (annualized %)", y = "gain (annualized %)", color = NULL) +
    ggtitle("Efficient Frontier: Gain vs Risk by Allocation") +
    app_theme$base_theme()

  if (show_labels) {
    plt <- plt +
      geom_text_repel(data = rebal_df,
                aes(x = sd, y = gain,
                    label = paste0(round(allocation * 100), "%")),
                size = app_theme$frontier_label_size,
                color = app_theme$frontier_label_color,
                nudge_y = diff(range(plot_df$gain)) * 0.04,
                segment.color = "gray70", segment.size = 0.3,
                min.segment.length = 0,
                box.padding = 0.3, point.padding = 0.2,
                max.overlaps = 20, seed = 42) +
      labs(caption = "Labels show stock allocation") +
      theme(plot.caption = element_text(face = "italic", size = 9,
                                        color = "#9a8e82", hjust = 0))
  }

  plt
}

#' Frontier sim explorer: portfolio value over time (rebal vs nonrebal)
plot_frontier_sim_portfolio <- function(portfolio_df, sim_label,
                                        show_rebal = TRUE,
                                        show_nonrebal = TRUE) {
  df <- data.frame(
    time = rep(portfolio_df$time, 2),
    value = c(portfolio_df$rebal_total, portfolio_df$nonrebal_total),
    strategy = rep(c("rebal", "nonrebal"), each = nrow(portfolio_df))
  )
  keep <- (df$strategy == "rebal" & show_rebal) |
          (df$strategy == "nonrebal" & show_nonrebal)
  df <- df[keep, ]
  ggplot(df, aes(x = time, y = value, color = strategy)) +
    geom_line() +
    scale_color_manual(values = app_theme$strategy_colors,
                       labels = app_theme$strategy_labels) +
    labs(x = "day", y = "portfolio value ($)", color = NULL) +
    ggtitle(sim_label) +
    app_theme$base_theme()
}

#' Frontier sim explorer: detail subplot (shares held + underlying prices)
plot_frontier_sim_detail <- function(portfolio_df, mean_stock, mean_bond, sim_label) {
  # Panel 1: Rebalanced shares held
  shares_df <- data.frame(
    time = rep(portfolio_df$time, 2),
    shares = c(portfolio_df$rebal_stock_shares, portfolio_df$rebal_bond_shares),
    class = rep(c("stocks", "bonds"), each = nrow(portfolio_df))
  )
  p1 <- ggplot(shares_df, aes(x = time, y = shares, color = class)) +
    geom_line() +
    scale_color_manual(values = app_theme$asset_colors) +
    labs(y = "shares", color = NULL) +
    ggtitle(paste("Shares Held (Rebalanced);", sim_label)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10), axis.title.x = element_blank())

  # Panel 2: Underlying prices
  prices_df <- data.frame(
    time = rep(portfolio_df$time, 2),
    price = c(portfolio_df$stock_price, portfolio_df$bond_price),
    class = rep(c("stocks", "bonds"), each = nrow(portfolio_df))
  )
  mean_df <- data.frame(
    time = rep(portfolio_df$time, 2),
    price = c(mean_stock, mean_bond),
    class = rep(c("stocks", "bonds"), each = nrow(portfolio_df))
  )
  p2 <- ggplot() +
    geom_line(data = prices_df, aes(x = time, y = price, color = class)) +
    geom_line(data = mean_df, aes(x = time, y = price, color = class),
              alpha = app_theme$mean_line_alpha, linetype = "dashed") +
    scale_color_manual(values = app_theme$asset_colors) +
    labs(x = "day", y = "cost per share ($)", color = NULL) +
    ggtitle(paste("Underlying Prices;", sim_label)) +
    app_theme$base_theme() +
    theme(plot.title = element_text(size = 10))

  # Combine as subplot
  pp1 <- ggplotly(p1)
  pp2 <- ggplotly(p2)
  for (i in seq_along(pp2$x$data)) pp2$x$data[[i]]$showlegend <- FALSE

  subplot(pp1, pp2, nrows = 2, shareX = TRUE, heights = c(0.5, 0.5), titleY = TRUE) %>%
    layout(
      showlegend = TRUE,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = 1.02,
                    font = list(color = app_theme$text_color)),
      paper_bgcolor = app_theme$panel_bg,
      plot_bgcolor = app_theme$panel_bg
    )
}
