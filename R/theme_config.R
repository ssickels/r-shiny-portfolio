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
