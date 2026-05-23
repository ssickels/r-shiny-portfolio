# scripts/screenshots.R — Dev-only tool for generating frontier plot screenshots
# Run: Rscript scripts/screenshots.R
#
# Generates full-size PNGs with toggleable layers. Crop/scale for thumbnails
# externally (e.g. Preview, ImageMagick).

library(ggplot2)

source("R/theme_config.R")

# --- CONFIGURE ---
rds_path    <- "data/precomputed/frontier_corrn030_rebal100_n5000.rds"
alloc_index <- 6        # 1-11 (index into sorted allocations)
out_dir     <- "screenshots/dev"

# Output dimensions (pixels)
out_width  <- 1200
out_height <- 800
out_dpi    <- 150

# Zoom box (NULL = auto-fit from data)
xlim <- c(0.05, 0.35)
ylim <- c(-0.02, 0.12)

# Layer toggles
show_cloud_rebal         <- TRUE
show_cloud_nonrebal      <- TRUE
show_dots_rebal          <- TRUE   # current alloc PE dot
show_dots_rebal_other    <- TRUE   # other alloc PE dots
show_dots_nonrebal       <- TRUE
show_dots_nonrebal_other <- TRUE
show_line_rebal          <- TRUE   # frontier connecting line
show_line_nonrebal       <- TRUE
show_labels              <- FALSE
cloud_alpha              <- 0.08

# Batch mode: generate all meaningful layer combinations
batch <- FALSE

# --- DATA PREP ---
sweep_data <- readRDS(rds_path)
allocs <- sort(unique(sweep_data$point_estimates$allocation))
current_alloc <- allocs[alloc_index]

all_pe <- sweep_data$point_estimates
cloud_all <- sweep_data$cloud

# Split data into named pieces
cloud_rebal    <- cloud_all[cloud_all$allocation == current_alloc & cloud_all$type == "rebal", ]
cloud_nonrebal <- cloud_all[cloud_all$allocation == current_alloc & cloud_all$type == "nonrebal", ]

pe_rebal_current    <- all_pe[all_pe$allocation == current_alloc & all_pe$type == "rebal", ]
pe_rebal_other      <- all_pe[all_pe$allocation != current_alloc & all_pe$type == "rebal", ]
pe_nonrebal_current <- all_pe[all_pe$allocation == current_alloc & all_pe$type == "nonrebal", ]
pe_nonrebal_other   <- all_pe[all_pe$allocation != current_alloc & all_pe$type == "nonrebal", ]

frontier_path_rebal    <- all_pe[all_pe$type == "rebal", ]
frontier_path_nonrebal <- all_pe[all_pe$type == "nonrebal", ]

# Label data (rebal frontier points only)
label_data <- frontier_path_rebal

# --- PLOT BUILDER ---
build_frontier_screenshot <- function(
    show_cloud_rebal, show_cloud_nonrebal,
    show_dots_rebal, show_dots_rebal_other,
    show_dots_nonrebal, show_dots_nonrebal_other,
    show_line_rebal, show_line_nonrebal,
    show_labels, cloud_alpha,
    xlim, ylim
) {
  plt <- ggplot()

  # 1. Cloud rebal
  if (show_cloud_rebal) {
    plt <- plt +
      geom_point(data = cloud_rebal, aes(x = sd, y = gain, color = type),
                 alpha = cloud_alpha, show.legend = FALSE)
  }

  # 2. Cloud nonrebal
  if (show_cloud_nonrebal) {
    plt <- plt +
      geom_point(data = cloud_nonrebal, aes(x = sd, y = gain, color = type),
                 alpha = cloud_alpha, show.legend = FALSE)
  }

  # 3. Frontier path rebal
  if (show_line_rebal) {
    plt <- plt +
      geom_path(data = frontier_path_rebal, aes(x = sd, y = gain),
                color = app_theme$frontier_line_color,
                alpha = app_theme$frontier_line_alpha)
  }

  # 4. Frontier path nonrebal
  if (show_line_nonrebal) {
    plt <- plt +
      geom_path(data = frontier_path_nonrebal, aes(x = sd, y = gain),
                color = app_theme$frontier_line_color,
                alpha = app_theme$frontier_line_alpha)
  }

  # 5. Other alloc PE dots rebal
  if (show_dots_rebal_other) {
    plt <- plt +
      geom_point(data = pe_rebal_other, aes(x = sd, y = gain, color = type),
                 size = 2, alpha = app_theme$other_point_alpha)
  }

  # 6. Other alloc PE dots nonrebal
  if (show_dots_nonrebal_other) {
    plt <- plt +
      geom_point(data = pe_nonrebal_other, aes(x = sd, y = gain, color = type),
                 size = 2, alpha = app_theme$other_point_alpha)
  }

  # 7. Current alloc PE dot rebal
  if (show_dots_rebal) {
    plt <- plt +
      geom_point(data = pe_rebal_current, aes(x = sd, y = gain, color = type),
                 size = app_theme$current_point_size,
                 alpha = app_theme$current_point_alpha)
  }

  # 8. Current alloc PE dot nonrebal
  if (show_dots_nonrebal) {
    plt <- plt +
      geom_point(data = pe_nonrebal_current, aes(x = sd, y = gain, color = type),
                 size = app_theme$current_point_size,
                 alpha = app_theme$current_point_alpha)
  }

  # 9. Allocation labels
  if (show_labels) {
    plt <- plt +
      geom_text(data = label_data,
                aes(x = sd, y = gain,
                    label = paste0(round(allocation * 100), "%")),
                size = app_theme$frontier_label_size, vjust = -1,
                color = app_theme$frontier_label_color)
  }

  # Zoom
  if (!is.null(xlim) && !is.null(ylim)) {
    plt <- plt + coord_cartesian(xlim = xlim, ylim = ylim)
  }

  # Scales and theme
  plt <- plt +
    scale_color_manual(values = app_theme$strategy_colors) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "volatility (annualized %)", y = "gain (annualized %)", color = NULL) +
    app_theme$base_theme()

  plt
}

# --- SAVE HELPER ---
save_plot <- function(plt, name) {
  fname <- file.path(out_dir, paste0(name, ".png"))
  ggsave(fname, plt,
         width = out_width / out_dpi, height = out_height / out_dpi,
         dpi = out_dpi, bg = app_theme$panel_bg)
  message("  saved: ", fname)
}

# --- BATCH PERMUTATIONS ---
run_all_permutations <- function() {
  combos <- list(
    clouds_only    = list(cr = TRUE,  cn = TRUE,  dr = FALSE, dro = FALSE, dn = FALSE, dno = FALSE, lr = FALSE, ln = FALSE, lab = FALSE),
    frontier_only  = list(cr = FALSE, cn = FALSE, dr = FALSE, dro = TRUE,  dn = FALSE, dno = TRUE,  lr = TRUE,  ln = TRUE,  lab = TRUE),
    dots_only      = list(cr = FALSE, cn = FALSE, dr = TRUE,  dro = TRUE,  dn = TRUE,  dno = TRUE,  lr = FALSE, ln = FALSE, lab = FALSE),
    rebal_only     = list(cr = TRUE,  cn = FALSE, dr = TRUE,  dro = TRUE,  dn = FALSE, dno = FALSE, lr = TRUE,  ln = FALSE, lab = FALSE),
    nonrebal_only  = list(cr = FALSE, cn = TRUE,  dr = FALSE, dro = FALSE, dn = TRUE,  dno = TRUE,  lr = FALSE, ln = TRUE,  lab = FALSE),
    full           = list(cr = TRUE,  cn = TRUE,  dr = TRUE,  dro = TRUE,  dn = TRUE,  dno = TRUE,  lr = TRUE,  ln = TRUE,  lab = FALSE),
    full_labels    = list(cr = TRUE,  cn = TRUE,  dr = TRUE,  dro = TRUE,  dn = TRUE,  dno = TRUE,  lr = TRUE,  ln = TRUE,  lab = TRUE)
  )

  for (combo_name in names(combos)) {
    cc <- combos[[combo_name]]
    plt <- build_frontier_screenshot(
      show_cloud_rebal = cc$cr, show_cloud_nonrebal = cc$cn,
      show_dots_rebal = cc$dr, show_dots_rebal_other = cc$dro,
      show_dots_nonrebal = cc$dn, show_dots_nonrebal_other = cc$dno,
      show_line_rebal = cc$lr, show_line_nonrebal = cc$ln,
      show_labels = cc$lab, cloud_alpha = cloud_alpha,
      xlim = xlim, ylim = ylim
    )
    save_plot(plt, paste0("frontier_", combo_name))
  }
}

# --- RUN ---
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (batch) {
  message("Batch mode: generating all permutations...")
  run_all_permutations()
} else {
  plt <- build_frontier_screenshot(
    show_cloud_rebal, show_cloud_nonrebal,
    show_dots_rebal, show_dots_rebal_other,
    show_dots_nonrebal, show_dots_nonrebal_other,
    show_line_rebal, show_line_nonrebal,
    show_labels, cloud_alpha,
    xlim, ylim
  )
  save_plot(plt, "frontier")
}

message("Done.")
