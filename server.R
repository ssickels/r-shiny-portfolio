# server.R — Shiny server logic

function(input, output, session) {

  # Hardcoded parameters (not exposed in UI)
  s.mean <- 10
  b.mean <- 10
  init.inv <- 1000
  propStepDivisor <- 20
  int.period <- 300

  # Screenshot mode: set to a sim number to highlight it on the Gain vs SD plot.
  # Set to NULL for normal operation. Set screenshot_hide_cloud to TRUE to show
  # only point estimates (for "point estimate only" screenshot).
  screenshot_highlight_sim <- NULL
  screenshot_hide_cloud <- FALSE

  # Reactive values to store results
  sim_result <- reactiveVal(NULL)
  sweep_result <- reactiveVal(NULL)
  frontier_alloc_index <- reactiveVal(6)  # default to 50/50 (index 6 of 0%,10%,...,100%)
  frontier_highlight_sim <- reactiveVal(NULL)
  active_scenario <- reactiveVal("corrn030_rebal100")  # tracks which precomputed scenario is loaded

  # --- Manage params_details open/closed state based on active tab ---
  # In Single allocation mode: force open, hide summary (looks like regular content)
  # In Frontier mode: show summary, collapse by default
  observe({
    req(input$main_tabs)
    if (input$main_tabs == "Single allocation") {
      shinyjs::runjs("
        var el = document.getElementById('params_details');
        if (el) el.setAttribute('open', '');
        var s = document.getElementById('params_summary');
        if (s) { s.style.display = ''; s.classList.add('params-static'); }
      ")
    } else {
      shinyjs::runjs("
        var el = document.getElementById('params_details');
        if (el) el.removeAttribute('open');
        var s = document.getElementById('params_summary');
        if (s) { s.style.display = ''; s.classList.remove('params-static'); }
      ")
    }
  })

  # --- Cloud alpha: single allocation slider ---
  observe({
    alpha <- input$cloud_alpha_single
    req(!is.null(alpha))
    if (alpha == 0) {
      shinyjs::runjs("$('#cloud_alpha_single').closest('.form-group').addClass('cloud-hidden-warning')")
    } else {
      shinyjs::runjs("$('#cloud_alpha_single').closest('.form-group').removeClass('cloud-hidden-warning')")
    }
  })

  observe({
    alpha <- input$cloud_alpha_frontier
    req(!is.null(alpha))
    if (alpha == 0) {
      shinyjs::runjs("$('#cloud_alpha_frontier').closest('.form-group').addClass('cloud-hidden-warning')")
    } else {
      shinyjs::runjs("$('#cloud_alpha_frontier').closest('.form-group').removeClass('cloud-hidden-warning')")
    }
  })

  # Update cloud opacity in-place (no re-render, preserves zoom)
  observeEvent(input$cloud_alpha_single, {
    alpha <- input$cloud_alpha_single
    plotlyProxy("plot_gain_sd", session) %>%
      plotlyProxyInvoke("restyle", list(`marker.opacity` = alpha), list(0L, 1L))
  }, ignoreInit = TRUE)

  observeEvent(input$cloud_alpha_frontier, {
    alpha <- input$cloud_alpha_frontier
    plotlyProxy("plot_frontier_merged", session) %>%
      plotlyProxyInvoke("restyle", list(`marker.opacity` = alpha), list(0L, 1L))
  }, ignoreInit = TRUE)

  # Data source tracking: "precomputed" or "custom"
  single_source <- reactiveVal("precomputed")
  sweep_source <- reactiveVal("precomputed")

  # --- Conditional visibility of restore/reset buttons ---
  observe({
    req(input$main_tabs)
    is_custom <- (input$main_tabs == "Single allocation" && single_source() == "custom") ||
                 (input$main_tabs == "Frontier" && sweep_source() == "custom")
    shinyjs::toggle("restore_precomputed", condition = is_custom)
  })

  observe({
    # Show "Reset to defaults" only when at least one param differs
    defaults_match <- isTRUE(input$nSims == 500) &&
                      isTRUE(input$n_timesteps == 1000) &&
                      isTRUE(input$perc_stocks == 60) &&
                      isTRUE(input$rebal_interval == 100) &&
                      isTRUE(input$s_sd == 6) && isTRUE(input$b_sd == 3) &&
                      isTRUE(input$s_b_corr == 0.7) &&
                      isTRUE(input$s_int == 0.06) && isTRUE(input$b_int == 0.02)
    shinyjs::toggle("reset_defaults", condition = !defaults_match)
  })

  # --- Deep-link: switch to a specific tab if ?tab= is in the URL ---
  observe({
    query <- parseQueryString(session$clientData$url_search)
    tab <- query$tab
    if (!is.null(tab)) {
      if (tab %in% c("frontier_explorer", "efficient_frontier")) {
        updateTabsetPanel(session, "main_tabs", selected = "Frontier")
        if (tab == "frontier_explorer") {
          updateCheckboxInput(session, "show_distributions", value = TRUE)
        }
      } else if (tab %in% c("gain_vs_sd", "final_densities", "trajectories",
                             "cost_profiles", "single_sim_explorer", "summary")) {
        updateTabsetPanel(session, "main_tabs", selected = "Single allocation")
        sub <- switch(tab,
          gain_vs_sd = "Gain vs SD",
          final_densities = "Final Densities",
          trajectories = "Trajectories",
          cost_profiles = "Cost Profiles",
          single_sim_explorer = "Single Sim Explorer",
          summary = "Summary"
        )
        updateTabsetPanel(session, "single_tabs", selected = sub)
      }
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # --- Auto-load precomputed data on startup ---
  observe({
    single_path <- "data/precomputed/single_n200.rds"
    frontier_path <- "data/precomputed/frontier_corrn030_rebal100_n5000.rds"

    if (file.exists(single_path)) {
      sim_result(readRDS(single_path))
      single_source("precomputed")
    }
    if (file.exists(frontier_path)) {
      sweep_result(readRDS(frontier_path))
      sweep_source("precomputed")
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # --- Load precomputed frontier via scenario grid ---
  load_scenario <- function(corr_str, rebal) {
    fname <- sprintf("data/precomputed/frontier_corr%s_rebal%d_n5000.rds", corr_str, rebal)
    if (file.exists(fname)) {
      data <- readRDS(fname)
      sweep_result(data)
      sweep_source("precomputed")
      active_scenario(paste0("corr", corr_str, "_rebal", rebal))
      frontier_highlight_sim(NULL)
      shinyjs::hide("clear_highlight")
      n_allocs <- length(unique(data$point_estimates$allocation))
      if (frontier_alloc_index() > n_allocs) frontier_alloc_index(1)
    }
  }

  # Scenario grid button observers
  scenarios <- expand.grid(
    corr = c("070", "030", "000", "n030", "n070"),
    rebal = c(25, 100, 300), stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(scenarios))) {
    local({
      cs <- scenarios$corr[i]; rb <- scenarios$rebal[i]
      observeEvent(input[[paste0("scenario_", cs, "_", rb)]], {
        load_scenario(cs, rb)
      })
    })
  }

  # Highlight active scenario button
  observe({
    current <- active_scenario()
    # Grid buttons
    for (i in seq_len(nrow(scenarios))) {
      btn_id <- paste0("scenario_", scenarios$corr[i], "_", scenarios$rebal[i])
      scenario_key <- paste0("corr", scenarios$corr[i], "_rebal", scenarios$rebal[i])
      if (startsWith(current, scenario_key)) {
        shinyjs::addClass(btn_id, "scenario-active")
      } else {
        shinyjs::removeClass(btn_id, "scenario-active")
      }
    }
  })

  # --- Restore precomputed data ---
  observeEvent(input$restore_precomputed, {
    single_path <- "data/precomputed/single_n200.rds"
    frontier_path <- "data/precomputed/frontier_corrn030_rebal100_n5000.rds"

    if (file.exists(single_path)) {
      sim_result(readRDS(single_path))
      single_source("precomputed")
    }
    if (file.exists(frontier_path)) {
      sweep_result(readRDS(frontier_path))
      sweep_source("precomputed")
      active_scenario("corrn030_rebal100")
      frontier_alloc_index(1)
    }
  })

  # --- Reset all sliders to defaults ---
  observeEvent(input$reset_defaults, {
    updateSliderInput(session, "nSims", value = 500)
    updateSliderInput(session, "n_timesteps", value = 1000)
    updateSliderInput(session, "perc_stocks", value = 60)
    updateSliderInput(session, "rebal_interval", value = 100)
    updateSliderInput(session, "cloud_alpha_single", value = 0.08)
    updateSliderInput(session, "cloud_alpha_frontier", value = 0.08)
    updateSliderInput(session, "s_sd", value = 6)
    updateSliderInput(session, "b_sd", value = 3)
    updateSliderInput(session, "s_b_corr", value = 0.7)
    updateSliderInput(session, "s_int", value = 0.06)
    updateSliderInput(session, "b_int", value = 0.02)
    updateCheckboxInput(session, "show_distributions", value = FALSE)
    updateCheckboxInput(session, "zoom_to_frontier", value = TRUE)
  })

  # --- Status banner (single allocation only; frontier info goes in plot title) ---
  output$data_status_banner <- renderUI({
    tab <- input$main_tabs

    if (tab == "Single allocation") {
      src <- single_source()
      res <- sim_result()
      if (is.null(res)) return(NULL)
      nsims <- res$params$nSims
      if (src == "precomputed") {
        msg <- paste0("Showing precomputed results (", nsims,
                       " sims, 60/40 allocation). ",
                       "Adjust parameters and run your own, or switch to Frontier ",
                       "to explore precomputed scenarios.")
      } else {
        msg <- paste0("Showing custom simulation results (", nsims, " sims).")
      }
      div(class = "data-status-banner", msg)
    } else {
      NULL
    }
  })

  # --- Frontier scenario label (reactive for plot titles) ---
  frontier_scenario_label <- reactive({
    sc <- active_scenario()
    sw <- sweep_result()
    req(sw)

    if (grepl("^corr", sc)) {
      base <- sub("_n\\d+$", "", sc)
      parts <- regmatches(base, regexec("corr(n?\\d+)_rebal(\\d+)", base))[[1]]
      corr_val <- if (substr(parts[2], 1, 1) == "n") {
        paste0("\u2212", as.numeric(sub("n", "", parts[2])) / 100)
      } else {
        as.character(as.numeric(parts[2]) / 100)
      }
      rebal <- parts[3]
      nsim_str <- if (grepl("_n\\d+$", sc)) {
        format(as.numeric(sub(".*_n", "", sc)), big.mark = ",")
      } else {
        "5,000"
      }
      paste0("corr ", corr_val, ", rebal ", rebal, "d, ", nsim_str, " sims")
    } else if (sweep_source() == "custom") {
      nsims <- nrow(sw$cloud) / length(unique(sw$cloud$allocation)) / 2
      paste0("custom run, ", format(round(nsims), big.mark = ","), " sims")
    } else {
      ""
    }
  })

  # --- Run single allocation button: show confirmation modal ---
  observeEvent(input$run_single, {
    nsims <- input$nSims
    est_seconds <- round(nsims * 0.1)

    showModal(modalDialog(
      title = "Run Single Allocation?",
      tagList(
        p(paste0("This will run ", format(nsims, big.mark = ","),
                 " simulations at ", input$perc_stocks, "% stocks / ",
                 100 - input$perc_stocks, "% bonds.")),
        p(tags$strong(paste0("Estimated time: ~", est_seconds, " seconds")),
          " (varies by hardware)."),
        p("This will replace the current single-allocation data. ",
          "You can restore precomputed results anytime.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_run", "Run", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # --- Run frontier button: show confirmation modal ---
  observeEvent(input$run_frontier, {
    nsims <- input$nSims
    n_allocs <- 11
    total_sims <- n_allocs * nsims
    est_minutes <- round(total_sims * 0.1 / 60, 1)

    showModal(modalDialog(
      title = "Run Frontier Sweep?",
      tagList(
        p(paste0("This will run ", format(total_sims, big.mark = ","),
                 " simulations (", n_allocs, " allocations \u00d7 ",
                 format(nsims, big.mark = ","), " sims each).")),
        p(tags$strong(paste0("Estimated time: ~", est_minutes, " minutes")),
          " (varies by hardware; frontier sweeps at 1,000+ sims can take much longer)."),
        p("This will replace the current frontier data. ",
          "You can restore precomputed results anytime.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_frontier_run", "Run", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # Helper: execute a frontier sweep with current parameters
  do_frontier_sweep <- function() {
    alloc_fracs <- clamp_pct_to_fraction(seq(0, 100, by = 10))
    total_work <- length(alloc_fracs) * input$nSims

    withProgress(message = "Running frontier sweep...", value = 0, {
      set.seed(100)
      result <- run_meta_sweep(
        perc.stocks.vec = alloc_fracs,
        n = input$n_timesteps,
        s.mean = s.mean,
        s.sd = input$s_sd,
        b.mean = b.mean,
        b.sd = input$b_sd,
        s.b.corr = input$s_b_corr,
        init.inv = init.inv,
        rebal.interval = input$rebal_interval,
        s.int = input$s_int,
        b.int = input$b_int,
        int.period = int.period,
        propStepDivisor = propStepDivisor,
        nSims = input$nSims,
        nSimsToRecord = min(5, input$nSims),
        progress_callback = function(alloc_i, total_allocs, sim_i, total_sims, msg) {
          done <- (alloc_i - 1) * total_sims + sim_i
          setProgress(value = done / total_work, message = msg)
        }
      )
      sweep_result(result)
      sweep_source("custom")
    })

    frontier_alloc_index(1)
    active_scenario("")
  }

  # --- Confirmed single allocation run ---
  observeEvent(input$confirm_run, {
    removeModal()
    shinyjs::disable("run_single")

    perc <- clamp_pct_to_fraction(input$perc_stocks)
    nSimsToRecord <- min(10, input$nSims)

    withProgress(message = "Running simulation...", value = 0, {
      set.seed(100)
      result <- run_simulation(
        n = input$n_timesteps,
        s.mean = s.mean,
        s.sd = input$s_sd,
        b.mean = b.mean,
        b.sd = input$b_sd,
        s.b.corr = input$s_b_corr,
        init.inv = init.inv,
        perc.stocks = perc,
        rebal.interval = input$rebal_interval,
        s.int = input$s_int,
        b.int = input$b_int,
        int.period = int.period,
        propStepDivisor = propStepDivisor,
        nSims = input$nSims,
        nSimsToRecord = nSimsToRecord,
        progress_callback = function(i, total, msg) {
          setProgress(value = i / total, message = msg)
        }
      )
      sim_result(result)
      single_source("custom")
    })

    shinyjs::enable("run_single")
  })

  # --- Confirmed frontier run ---
  observeEvent(input$confirm_frontier_run, {
    removeModal()
    do_frontier_sweep()
  })

  # --- Plotly config: download + reset axes ---
  plotly_clean <- function(p) {
    config(p, displayModeBar = TRUE,
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                       "zoomIn2d", "zoomOut2d",
                                       "hoverClosestCartesian",
                                       "hoverCompareCartesian", "toggleSpikelines"),
           displaylogo = FALSE)
  }

  # --- Plot outputs (plotly) ---

  output$plot_gain_sd <- renderPlotly({
    req(sim_result())
    plotly_clean(ggplotly(plot_gain_vs_sd(sim_result(),
      highlight_sim = screenshot_highlight_sim,
      show_cloud = !screenshot_hide_cloud,
      cloud_alpha_override = isolate(input$cloud_alpha_single))))
  })

  output$plot_trajectories <- renderPlotly({
    req(sim_result())
    plotly_clean(ggplotly(
      plot_trajectories(sim_result(), max_sims = min(100, sim_result()$params$nSims))
    ))
  })

  output$plot_costs <- renderPlotly({
    req(sim_result())
    plotly_clean(ggplotly(
      plot_cost_profiles(sim_result(), max_sims = min(10, sim_result()$params$nSims))
    ))
  })

  output$plot_densities <- renderPlotly({
    req(sim_result())
    plotly_clean(ggplotly(plot_final_densities(sim_result())))
  })

  # --- Single Sim Explorer ---
  sim_explorer_index <- reactiveVal(1)

  observeEvent(input$sim_prev, {
    idx <- sim_explorer_index()
    if (idx > 1) sim_explorer_index(idx - 1)
  })

  observeEvent(input$sim_next, {
    res <- sim_result()
    req(res)
    max_idx <- res$params$nSimsToRecord
    idx <- sim_explorer_index()
    if (idx < max_idx) sim_explorer_index(idx + 1)
  })

  output$sim_explorer_label <- renderText({
    res <- sim_result()
    req(res)
    paste0("Sim ", sim_explorer_index(), " of ", res$params$nSimsToRecord)
  })

  output$plot_explorer <- renderPlotly({
    req(sim_result())
    plotly_clean(plot_single_sim_explorer(sim_result(), sim_explorer_index()))
  })

  # --- Merged frontier plot ---

  # Store zoom state so it persists across allocation steps
  frontier_zoom <- reactiveValues(x = NULL, y = NULL)

  # Precomputed axis ranges: tight (PE only) and wide (includes cloud spread)
  frontier_axis_ranges <- reactive({
    sw <- sweep_result()
    req(sw)
    pe <- sw$point_estimates
    cl <- sw$cloud

    pe_x <- range(pe$sd)
    pe_y <- range(pe$gain)
    pe_x_pad <- diff(pe_x) * 0.08
    pe_y_pad <- diff(pe_y) * 0.08

    cl_x <- quantile(cl$sd, c(0.02, 0.98), na.rm = TRUE)
    cl_y <- quantile(cl$gain, c(0.02, 0.98), na.rm = TRUE)
    wide_x <- c(min(pe_x[1], cl_x[1]), max(pe_x[2], cl_x[2]))
    wide_y <- c(min(pe_y[1], cl_y[1]), max(pe_y[2], cl_y[2]))
    wide_x_pad <- diff(wide_x) * 0.05
    wide_y_pad <- diff(wide_y) * 0.05

    list(
      tight = list(x = pe_x + c(-pe_x_pad, pe_x_pad),
                   y = pe_y + c(-pe_y_pad, pe_y_pad)),
      wide  = list(x = wide_x + c(-wide_x_pad, wide_x_pad),
                   y = wide_y + c(-wide_y_pad, wide_y_pad))
    )
  })

  # Track current zoom preset so the toggle button knows which way to go
  frontier_zoom_mode <- reactiveVal("in")  # "in" = tight around PEs, "out" = wide for clouds

  # Checkbox: zoom to frontier (tight) vs full cloud range (wide)
  observeEvent(input$zoom_to_frontier, {
    ranges <- frontier_axis_ranges()
    if (isTRUE(input$zoom_to_frontier)) {
      frontier_zoom_mode("in")
      frontier_zoom$x <- ranges$tight$x
      frontier_zoom$y <- ranges$tight$y
    } else {
      frontier_zoom_mode("out")
      frontier_zoom$x <- ranges$wide$x
      frontier_zoom$y <- ranges$wide$y
    }
    shinyjs::hide("reset_view")
  }, ignoreInit = TRUE)

  # Capture zoom/pan events from plotly (user drag-zoom)
  observe({
    req(sweep_result())
    ev <- event_data("plotly_relayout", source = "frontier_src")
    req(ev)
    if (!is.null(ev[["xaxis.range[0]"]])) {
      frontier_zoom$x <- c(ev[["xaxis.range[0]"]], ev[["xaxis.range[1]"]])
      frontier_zoom$y <- c(ev[["yaxis.range[0]"]], ev[["yaxis.range[1]"]])
      shinyjs::show("reset_view")
    }
    if (isTRUE(ev[["xaxis.autorange"]]) || !is.null(ev[["autosize"]])) {
      frontier_zoom$x <- NULL
      frontier_zoom$y <- NULL
      shinyjs::hide("reset_view")
    }
  })

  # Reset view button: restores preset zoom based on checkbox state
  observeEvent(input$reset_view, {
    ranges <- frontier_axis_ranges()
    if (isTRUE(input$zoom_to_frontier)) {
      frontier_zoom$x <- ranges$tight$x
      frontier_zoom$y <- ranges$tight$y
    } else {
      frontier_zoom$x <- ranges$wide$x
      frontier_zoom$y <- ranges$wide$y
    }
    shinyjs::hide("reset_view")
  })

  # --- Highlight a random simulation pair ---
  observeEvent(input$highlight_random_sim, {
    sw <- sweep_result()
    req(sw)
    allocs <- sort(unique(sw$point_estimates$allocation))
    current_alloc <- allocs[frontier_alloc_index()]
    current_cloud <- sw$cloud[sw$cloud$allocation == current_alloc, ]
    sims <- unique(current_cloud$sim)
    current_hl <- frontier_highlight_sim()
    candidates <- setdiff(sims, current_hl)
    if (length(candidates) == 0) candidates <- sims
    new_sim <- sample(candidates, 1)
    frontier_highlight_sim(new_sim)
    shinyjs::show("clear_highlight")

    # Check if highlighted dots are within current viewport; zoom out if not
    hl <- current_cloud[current_cloud$sim == new_sim, ]
    if (nrow(hl) >= 2 && !is.null(frontier_zoom$x)) {
      xr <- frontier_zoom$x
      yr <- frontier_zoom$y
      dots_visible <- all(hl$sd >= xr[1] & hl$sd <= xr[2]) &&
                      all(hl$gain >= yr[1] & hl$gain <= yr[2])
      if (!dots_visible) {
        ranges <- frontier_axis_ranges()
        frontier_zoom_mode("out")
        frontier_zoom$x <- ranges$wide$x
        frontier_zoom$y <- ranges$wide$y
        updateCheckboxInput(session, "zoom_to_frontier", value = FALSE)
        shinyjs::hide("reset_view")
      }
    }
  })

  observeEvent(input$clear_highlight, {
    frontier_highlight_sim(NULL)
    shinyjs::hide("clear_highlight")
  })

  # When allocation changes, pick a new random sim if highlight is active
  observeEvent(frontier_alloc_index(), {
    if (!is.null(frontier_highlight_sim())) {
      sw <- sweep_result()
      req(sw)
      allocs <- sort(unique(sw$point_estimates$allocation))
      current_alloc <- allocs[frontier_alloc_index()]
      current_cloud <- sw$cloud[sw$cloud$allocation == current_alloc, ]
      sims <- unique(current_cloud$sim)
      frontier_highlight_sim(sample(sims, 1))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$frontier_prev, {
    idx <- frontier_alloc_index()
    if (idx > 1) frontier_alloc_index(idx - 1)
  })

  observeEvent(input$frontier_next, {
    sweep <- sweep_result()
    req(sweep)
    n_allocs <- length(unique(sweep$point_estimates$allocation))
    idx <- frontier_alloc_index()
    if (idx < n_allocs) frontier_alloc_index(idx + 1)
  })

  # Grey out frontier nav arrows at boundaries
  observe({
    sweep <- sweep_result()
    req(sweep)
    idx <- frontier_alloc_index()
    n_allocs <- length(unique(sweep$point_estimates$allocation))
    shinyjs::toggleState("frontier_prev", condition = idx > 1)
    shinyjs::toggleState("frontier_next", condition = idx < n_allocs)
  })

  output$plot_frontier_merged <- renderPlotly({
    req(sweep_result())
    scenario_lbl <- frontier_scenario_label()

    if (isTRUE(input$show_distributions)) {
      # Distribution view: clouds + frontier lines, no labels
      allocs <- sort(unique(sweep_result()$point_estimates$allocation))
      current_alloc <- allocs[frontier_alloc_index()]
      alloc_str <- paste0(round(current_alloc * 100, 1), "% Stocks / ",
                          round((1 - current_alloc) * 100, 1), "% Bonds")
      title <- paste0("Frontier Explorer: ", alloc_str)
      if (nchar(scenario_lbl) > 0) title <- paste0(title, " \u2014 ", scenario_lbl)

      plt <- plot_frontier_explorer(sweep_result(), frontier_alloc_index(),
                                     show_cloud = TRUE,
                                     cloud_alpha_override = isolate(input$cloud_alpha_frontier),
                                     show_labels = FALSE,
                                     highlight_sim = frontier_highlight_sim()) +
        ggtitle(title)

      p <- plotly_clean(ggplotly(plt, source = "frontier_src") %>%
                          event_register("plotly_relayout"))

      if (!is.null(frontier_zoom$x)) {
        p <- p %>% layout(
          xaxis = list(range = frontier_zoom$x),
          yaxis = list(range = frontier_zoom$y)
        )
      }
      p
    } else {
      # Point estimate view: efficient frontier with labels
      title <- "Efficient Frontier"
      if (nchar(scenario_lbl) > 0) title <- paste0(title, " \u2014 ", scenario_lbl)

      plt <- plot_efficient_frontier(sweep_result()$summary, show_labels = TRUE) +
        ggtitle(title)
      # Register frontier_src so the zoom observer doesn't warn
      p <- plotly_clean(ggplotly(plt, source = "frontier_src") %>%
                          event_register("plotly_relayout"))

      # Apply saved zoom (e.g. user clicked "Zoom out" to see cloud range)
      if (!is.null(frontier_zoom$x)) {
        p <- p %>% layout(
          xaxis = list(range = frontier_zoom$x),
          yaxis = list(range = frontier_zoom$y)
        )
      }
      p
    }
  })

  output$frontier_alloc_label <- renderText({
    sweep <- sweep_result()
    req(sweep)
    allocs <- sort(unique(sweep$point_estimates$allocation))
    idx <- frontier_alloc_index()
    paste0("Allocation ", idx, " of ", length(allocs), ": ",
           round(allocs[idx] * 100, 1), "% Stocks / ",
           round((1 - allocs[idx]) * 100, 1), "% Bonds")
  })

  # --- Summary table ---

  output$summary_table <- renderTable({
    req(sim_result())
    format_summary_table(sim_result()$gainsVsSD.point.estDf)
  })

  output$params_text <- renderPrint({
    req(sim_result())
    cat(format_params_summary(sim_result()$params))
  })

  # --- Plot callout boxes (shown for precomputed data) ---

  callout <- function(text) {
    div(class = "plot-callout", HTML(text))
  }

  callout_bullets <- function(items) {
    div(class = "plot-callout",
      tags$ul(class = "callout-bullets",
        lapply(items, function(item) tags$li(HTML(item)))
      )
    )
  }

  output$callout_gain_sd <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "Each dot is one simulation's outcome: annualized gain vs. its standard deviation.
       The simulator generates correlated stock and bond price paths via Metropolis-Hastings
       MCMC, then tracks both a rebalanced and non-rebalanced portfolio through each path.
       The highlighted points are the <strong>point estimates</strong> (means across all sims).
       The cloud around them is the full distribution &mdash; the spread tells you how
       uncertain any single outcome is."
    )
  })

  output$callout_densities <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "Density curves of final portfolio values after the full time horizon.
       Rebalanced and non-rebalanced strategies start from the same MCMC price paths
       but diverge as the rebalanced portfolio periodically reallocates to maintain
       its target stock/bond split. Wider distributions mean more outcome uncertainty."
    )
  })

  output$callout_trajectories <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "Each line is a single simulation's portfolio value over time &mdash; one for each
       MCMC price path. Rebalanced and non-rebalanced portfolios diverge over the same
       underlying paths. The fan of trajectories shows the range of possible outcomes,
       not just the average."
    )
  })

  output$callout_costs <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "Share prices generated by the Metropolis-Hastings chain for each simulation.
       Stock and bond prices are correlated (controlled by the stock-bond correlation
       parameter) and include a drift term from their respective interest rates.
       These are the raw price paths that drive all portfolio outcomes."
    )
  })

  output$callout_explorer <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "This entire panel &mdash; all three views &mdash; is the story behind
       <strong>a single pair of dots</strong> on the Gain vs SD plot. Each simulation
       produces two dots (rebalanced and non-rebalanced) from the same price paths;
       here you're seeing the machinery that produced them.
       Top panel: the dollar value of each asset class in the rebalanced portfolio,
       plus total values for both strategies. Middle panel: number of shares held &mdash;
       rebalancing causes these to shift as the portfolio sells winners and buys losers.
       Bottom panel: the underlying share prices (MCMC output) with dashed mean price
       overlays."
    )
  })

  output$callout_frontier_merged <- renderUI({
    req(sweep_result())
    if (sweep_source() != "precomputed") return(NULL)

    if (isTRUE(input$show_distributions)) {
      callout_bullets(c(
        "Each simulation produces two dots from the same price paths &mdash; one <strong>rebalanced</strong> (teal), one <strong>non-rebalanced</strong> (pink). The only difference is the portfolio strategy. <a href='#' onclick='$(\"#highlight_random_sim\").click(); return false;' class='callout-action-link'>Highlight a random simulation</a> (or click the button below) to see a single pair connected by a line.",
        "The frontier dots (larger, connected) are the <strong>means</strong> across all simulations for the selected allocation. Individual pairs vary widely &mdash; that spread is the real story.",
        "The gap between the rebalanced and non-rebalanced frontier lines is the rebalancing benefit <em>on average</em>.",
        "Use the <strong>Zoom to frontier</strong> checkbox in the sidebar to toggle between a tight view around the frontier means and the full cloud range. You can also drag a box on the plot to zoom to a custom region."
      ))
    } else {
      callout_bullets(c(
        "Each point is a <strong>point estimate</strong> (mean gain and mean SD) from 5,000 simulations at one stock/bond allocation.",
        "Toggle <strong>Show distributions</strong> in the sidebar to see the full simulation clouds these points are derived from.",
        "Two curves: one rebalanced, one non-rebalanced. Where rebalancing shifts the curve upward or leftward, it&rsquo;s improving the risk-return tradeoff.",
        "Use the <strong>Zoom to frontier</strong> checkbox in the sidebar to toggle between a tight view around the means and the full range. You can also drag a box on the plot to zoom to a custom region."
      ))
    }
  })
}
