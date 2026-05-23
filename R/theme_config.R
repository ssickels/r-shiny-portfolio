# theme_config.R — Centralized color, alpha, and size settings for all plots

app_theme <- list(
  # Page / Shiny app background
  page_bg        = "#f5f0e8",   # warm linen
  panel_bg       = "#faf8f4",   # plot panel (near-white)
  text_color     = "#3b2f2f",   # dark walnut
  text_secondary = "#6b5b4f",   # muted brown
  border_color   = "#d4cabb",   # soft tan

  # Sim cloud points
  cloud_alpha    = 0.08,

  # Trajectory / cost lines
  traj_alpha     = 0.1,
  cost_alpha     = 0.5,

  # Point estimates
  point_size     = 3,
  point_alpha    = 0.8,
  point_border   = "black",

  # Frontier
  frontier_line_color  = "gray50",
  frontier_line_alpha  = 0.4,
  frontier_label_color = "gray30",
  frontier_label_size  = 2.5,
  other_point_color    = "gray50",
  other_point_alpha    = 0.5,
  current_point_size   = 4,
  current_point_alpha  = 0.9,

  # Delta plot
  delta_color    = "blue",
  delta_alpha    = 0.2,

  # Density fill
  density_alpha  = 0.2,

  # Mean line overlay
  mean_line_alpha = 0.5,

  # Strategy colors (rebal vs nonrebal)
  rebal_color   = "#00BFC4",  # teal
  nonrebal_color = "#F8766D", # pink

  # Asset class colors (stocks vs bonds)
  stock_color = "#2ca02c",    # green
  bond_color  = "#ff7f0e",    # orange

  # Named vectors for scale_color_manual()
  strategy_colors = c("total rebal" = "#00BFC4", "total non-rebal" = "#F8766D",
                       "rebal" = "#00BFC4", "nonrebal" = "#F8766D"),
  strategy_labels = c("rebal" = "rebalanced", "nonrebal" = "non-rebalanced"),
  asset_colors    = c("stocks" = "#2ca02c", "bonds" = "#ff7f0e"),
  explorer_colors = c("stocks" = "#2ca02c", "bonds" = "#ff7f0e",
                       "total rebal" = "#00BFC4", "total non-rebal" = "#F8766D"),

  # Base ggplot theme (function)
  base_theme     = function() {
    theme_light() %+replace%
      theme(
        plot.background  = element_rect(fill = "#faf8f4", color = NA),
        panel.background = element_rect(fill = "#faf8f4", color = NA),
        text = element_text(color = "#3b2f2f"),
        axis.text = element_text(color = "#6b5b4f"),
        plot.title = element_text(color = "#3b2f2f")
      )
  }
)
