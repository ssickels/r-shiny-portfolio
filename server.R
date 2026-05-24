# server.R — Shiny server logic

function(input, output, session) {

  # Hardcoded parameters (not exposed in UI)
  s.mean <- 10
  b.mean <- 10
  init.inv <- 1000
  propStepDivisor <- 20
  int.period <- 300

  # Screenshot mode: set to a sim number to highlight it on the Gain vs Volatility plot.
  # Set to NULL for normal operation. Set screenshot_hide_cloud to TRUE to show
  # only point estimates (for "point estimate only" screenshot).
  screenshot_highlight_sim <- NULL
  screenshot_hide_cloud <- FALSE

  # Reactive values to store results
  sim_result <- reactiveVal(NULL)
  sweep_result <- reactiveVal(NULL)
  frontier_alloc_index <- reactiveVal(6)  # default to 50/50 (index 6 of 0%,10%,...,100%)
  frontier_highlight_sim <- reactiveVal(NULL)
  frontier_cached_sim_idx <- reactiveVal(0L)   # 0 = none, 1-10 = which cached sim
  frontier_sim_portfolio  <- reactiveVal(NULL)  # reconstructed portfolio data
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
    # Cloud traces are always first; compute indices from toggle state
    traces <- integer(0)
    if (!isFALSE(input$layer_cloud_rebal))    traces <- c(traces, length(traces))
    if (!isFALSE(input$layer_cloud_nonrebal)) traces <- c(traces, length(traces))
    if (length(traces) > 0) {
      plotlyProxy("plot_frontier_merged", session) %>%
        plotlyProxyInvoke("restyle", list(`marker.opacity` = alpha), as.list(traces))
    }
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
    # Show "Reset to defaults" only when a slider relevant to the current tab differs
    shared_match <- isTRUE(input$nSims == 500) &&
                    isTRUE(input$n_timesteps == 1000) &&
                    isTRUE(input$rebal_interval == 100) &&
                    isTRUE(input$s_sd == 6) && isTRUE(input$b_sd == 3) &&
                    isTRUE(input$s_b_corr == 0.7) &&
                    isTRUE(input$s_int == 0.06) && isTRUE(input$b_int == 0.02)
    if (isTRUE(input$main_tabs == "Single allocation")) {
      defaults_match <- shared_match && isTRUE(input$perc_stocks == 60)
    } else {
      defaults_match <- shared_match
    }
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
          updateTabsetPanel(session, "frontier_tabs", selected = "clouds")
        }
      } else if (tab %in% c("bands", "range_of_outcomes")) {
        updateTabsetPanel(session, "main_tabs", selected = "Frontier")
        updateTabsetPanel(session, "frontier_tabs", selected = "bands")
      } else if (tab %in% c("gain_vs_sd", "gain_vs_volatility",
                             "single_sim_explorer", "summary")) {
        updateTabsetPanel(session, "main_tabs", selected = "Single allocation")
        sub <- switch(tab,
          gain_vs_sd = "Gain vs Volatility",
          gain_vs_volatility = "Gain vs Volatility",
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
      frontier_cached_sim_idx(0L)
      frontier_sim_portfolio(NULL)
      shinyjs::hide("clear_highlight")
      shinyjs::hide("frontier_sim_explorer_panel")
      # Reset zoom so it re-initializes from checkbox with new data's axis ranges
      frontier_zoom$x <- NULL
      frontier_zoom$y <- NULL
      shinyjs::hide("reset_view")
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
      frontier_zoom$x <- NULL
      frontier_zoom$y <- NULL
      shinyjs::hide("reset_view")
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
    updateTabsetPanel(session, "frontier_tabs", selected = "means")
    updateCheckboxInput(session, "zoom_to_frontier", value = TRUE)
    frontier_cached_sim_idx(0L)
    frontier_sim_portfolio(NULL)
    shinyjs::hide("frontier_sim_explorer_panel")
  })

  # --- Reset layer toggles to defaults ---
  observeEvent(input$reset_layer_toggles, {
    updateCheckboxInput(session, "layer_cloud_rebal", value = TRUE)
    updateCheckboxInput(session, "layer_cloud_nonrebal", value = TRUE)
    updateSliderInput(session, "cloud_sample_n", value = 5000)
    updateCheckboxInput(session, "layer_line_rebal", value = TRUE)
    updateCheckboxInput(session, "layer_line_nonrebal", value = TRUE)
    updateSliderInput(session, "frontier_line_width", value = 0.5)
    updateSliderInput(session, "frontier_line_alpha", value = 0.4)
    updateCheckboxInput(session, "layer_dots_rebal", value = TRUE)
    updateCheckboxInput(session, "layer_dots_nonrebal", value = TRUE)
    updateCheckboxInput(session, "layer_highlight_current", value = TRUE)
    updateCheckboxInput(session, "layer_dots_others", value = TRUE)
    updateSliderInput(session, "dot_size", value = 2)
    updateCheckboxInput(session, "layer_labels", value = FALSE)
    updateCheckboxInput(session, "sim_show_rebal", value = TRUE)
    updateCheckboxInput(session, "sim_show_nonrebal", value = TRUE)
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
    frontier_zoom$x <- NULL
    frontier_zoom$y <- NULL
    shinyjs::hide("reset_view")
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

  # --- Bands y-axis ranges (cached per scenario, stable across band type/strategy toggles) ---
  bands_ylim <- reactive({
    sw <- sweep_result()
    req(sw)
    compute_bands_ylim(sw$cloud)
  })

  # --- Merged frontier plot ---

  # Store zoom state so it persists across allocation steps
  frontier_zoom <- reactiveValues(x = NULL, y = NULL)
  # Timestamp of last programmatic zoom change; relayout events within a short

  # window after this are from re-renders, not user drag-zooms
  frontier_zoom_set_time <- reactiveVal(0)

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

  # Initialize zoom from checkbox when sweep data first becomes available
  observe({
    ranges <- frontier_axis_ranges()
    # Only initialize if zoom has never been set
    if (is.null(frontier_zoom$x)) {
      if (isTRUE(input$zoom_to_frontier)) {
        frontier_zoom$x <- ranges$tight$x
        frontier_zoom$y <- ranges$tight$y
      } else {
        frontier_zoom$x <- ranges$wide$x
        frontier_zoom$y <- ranges$wide$y
      }
      frontier_zoom_set_time(as.numeric(Sys.time()))
    }
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
    frontier_zoom_set_time(as.numeric(Sys.time()))
    shinyjs::hide("reset_view")
  }, ignoreInit = TRUE)

  # Capture zoom/pan events from plotly (user drag-zoom only)
  observe({
    req(sweep_result())
    ev <- event_data("plotly_relayout", source = "frontier_src")
    req(ev)
    # Ignore relayout events that fire within 1 second of a programmatic zoom change;
    # these are from re-renders, not user drag-zooms
    elapsed <- as.numeric(Sys.time()) - frontier_zoom_set_time()
    if (elapsed < 1) return()
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

  # Precomputed mean price vectors for frontier sim explorer detail plot
  frontier_mean_prices <- reactive({
    # Fixed params: s.mean=10, b.mean=10, s.int=0.06, b.int=0.02, int.period=300
    # n = 999 (after the n-1 adjustment in run_simulation)
    subsample_times <- c(1L, seq(5L, 999L, by = 5L))
    s_mean_vec <- 10 * ((1 + 0.06)^((subsample_times - 1) / 300))
    b_mean_vec <- 10 * ((1 + 0.02)^((subsample_times - 1) / 300))
    list(stock = s_mean_vec, bond = b_mean_vec)
  })

  # --- Highlight a simulation pair (cycling through cached sims) ---
  observeEvent(input$highlight_random_sim, {
    sw <- sweep_result()
    req(sw, sw$sim_histories)

    allocs <- sort(unique(sw$point_estimates$allocation))
    current_alloc <- allocs[frontier_alloc_index()]

    # Cycle through cached sims 1->2->...->N->1
    idx <- frontier_cached_sim_idx()
    n_cached <- length(unique(
      sw$sim_histories$sim[sw$sim_histories$allocation == current_alloc]
    ))
    new_idx <- if (idx >= n_cached) 1L else idx + 1L
    frontier_cached_sim_idx(new_idx)
    frontier_highlight_sim(new_idx)
    updateActionButton(session, "highlight_random_sim", label = "Highlight another simulation")
    shinyjs::show("clear_highlight")

    # Reconstruct portfolio from cached prices
    hist <- sw$sim_histories[
      sw$sim_histories$allocation == current_alloc &
      sw$sim_histories$sim == new_idx, ]

    # Get rebal interval from scenario params
    sc <- active_scenario()
    rebal_int <- as.integer(sub(".*_rebal(\\d+).*", "\\1", sc))
    if (is.na(rebal_int)) rebal_int <- 100L  # fallback for custom runs

    portfolio <- reconstruct_portfolio(
      stock_prices = hist$stock_price,
      bond_prices = hist$bond_price,
      time_vec = hist$time,
      perc_stocks = current_alloc,
      rebal_interval = rebal_int,
      init_inv = 1000
    )
    frontier_sim_portfolio(portfolio)
    shinyjs::show("frontier_sim_explorer_panel")

    # Check if highlighted dots are within current viewport; zoom out if not
    current_cloud <- sw$cloud[sw$cloud$allocation == current_alloc, ]
    hl <- current_cloud[current_cloud$sim == new_idx, ]
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
    frontier_cached_sim_idx(0L)
    frontier_sim_portfolio(NULL)
    updateActionButton(session, "highlight_random_sim", label = "Highlight a simulation")
    shinyjs::hide("clear_highlight")
    shinyjs::hide("frontier_sim_explorer_panel")
  })

  # When allocation changes, clear highlight and hide explorer
  observeEvent(frontier_alloc_index(), {
    if (!is.null(frontier_highlight_sim())) {
      frontier_highlight_sim(NULL)
      frontier_cached_sim_idx(0L)
      frontier_sim_portfolio(NULL)
      shinyjs::hide("clear_highlight")
      shinyjs::hide("frontier_sim_explorer_panel")
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
    # Mark render time so the relayout observer ignores the initial axis event
    frontier_zoom_set_time(as.numeric(Sys.time()))
    scenario_lbl <- frontier_scenario_label()

    if (identical(input$frontier_tabs, "clouds")) {
      # Distribution view: clouds + frontier lines, no labels
      allocs <- sort(unique(sweep_result()$point_estimates$allocation))
      current_alloc <- allocs[frontier_alloc_index()]
      alloc_str <- paste0(round(current_alloc * 100, 1), "% Stocks / ",
                          round((1 - current_alloc) * 100, 1), "% Bonds")
      title <- paste0("Frontier Explorer: ", alloc_str)
      if (nchar(scenario_lbl) > 0) title <- paste0(title, " \u2014 ", scenario_lbl)

      plt <- plot_frontier_explorer(sweep_result(), frontier_alloc_index(),
                                     cloud_alpha_override = isolate(input$cloud_alpha_frontier),
                                     show_labels = !isFALSE(input$layer_labels),
                                     highlight_sim = frontier_highlight_sim(),
                                     show_cloud_rebal = !isFALSE(input$layer_cloud_rebal),
                                     show_cloud_nonrebal = !isFALSE(input$layer_cloud_nonrebal),
                                     show_line_rebal = !isFALSE(input$layer_line_rebal),
                                     show_line_nonrebal = !isFALSE(input$layer_line_nonrebal),
                                     show_dots_rebal = !isFALSE(input$layer_dots_rebal),
                                     show_dots_nonrebal = !isFALSE(input$layer_dots_nonrebal),
                                     highlight_current = !isFALSE(input$layer_highlight_current),
                                     show_dots_others = !isFALSE(input$layer_dots_others),
                                     dot_size = if (is.null(input$dot_size)) 2 else input$dot_size,
                                     line_width = if (is.null(input$frontier_line_width)) 0.5 else input$frontier_line_width,
                                     line_alpha = if (is.null(input$frontier_line_alpha)) 0.4 else input$frontier_line_alpha,
                                     cloud_sample_n = input$cloud_sample_n) +
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
    } else if (identical(input$frontier_tabs, "bands")) {
      # Range of outcomes: percentile bands by allocation
      title <- "Range of Outcomes by Allocation"
      if (nchar(scenario_lbl) > 0) title <- paste0(title, " \u2014 ", scenario_lbl)

      bt <- if (is.null(input$band_type)) "percentile" else input$band_type
      bands_df <- compute_frontier_bands(sweep_result()$cloud, band_type = bt)
      plotly_clean(plot_frontier_bands(bands_df, title = title,
        show_rebal = !isFALSE(input$bands_show_rebal),
        show_nonrebal = !isFALSE(input$bands_show_nonrebal),
        ylim = bands_ylim()))
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

  # --- Frontier sim explorer outputs ---

  output$plot_frontier_sim_values <- renderPlotly({
    portfolio <- frontier_sim_portfolio()
    req(portfolio)
    idx <- frontier_cached_sim_idx()
    sw <- sweep_result()
    allocs <- sort(unique(sw$point_estimates$allocation))
    current_alloc <- allocs[frontier_alloc_index()]
    n_cached <- length(unique(
      sw$sim_histories$sim[sw$sim_histories$allocation == current_alloc]
    ))
    label <- paste0("Sim ", idx, " of ", n_cached, ": portfolio value over time")
    plotly_clean(ggplotly(plot_frontier_sim_portfolio(portfolio, label,
      show_rebal = !isFALSE(input$sim_show_rebal),
      show_nonrebal = !isFALSE(input$sim_show_nonrebal))))
  })

  output$plot_frontier_sim_detail <- renderPlotly({
    portfolio <- frontier_sim_portfolio()
    req(portfolio)
    idx <- frontier_cached_sim_idx()
    means <- frontier_mean_prices()
    label <- paste0("Sim ", idx)
    plotly_clean(plot_frontier_sim_detail(portfolio, means$stock, means$bond, label))
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

  output$callout_explorer <- renderUI({
    req(sim_result())
    if (single_source() != "precomputed") return(NULL)
    callout(
      "This entire panel &mdash; all three views &mdash; is the story behind
       <strong>a single pair of dots</strong> on the Gain vs Volatility plot. Each simulation
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

    if (identical(input$frontier_tabs, "clouds")) {
      callout_bullets(c(
        "Each simulation produces two dots from the same price paths &mdash; one <strong>rebalanced</strong> (teal), one <strong>non-rebalanced</strong> (pink). The only difference is the portfolio strategy. Click <strong>Highlight a simulation</strong> to see a single pair connected by a line and the underlying simulated history below.",
        "The frontier dots (larger, connected) are the <strong>means</strong> across all simulations for the selected allocation. Individual pairs vary widely &mdash; that spread is the real story.",
        "The gap between the rebalanced and non-rebalanced frontier lines is the rebalancing benefit <em>on average</em>.",
        "Use <strong>Zoom to frontier</strong> in the sidebar to toggle between a tight view around the frontier means and the full cloud range. You can also drag a box on the plot to zoom to a custom region."
      ))
    } else if (identical(input$frontier_tabs, "bands")) {
      callout_bullets(c(
        "Each strategy is summarized by two lines: a <strong>solid line</strong> for the mean (same as the means plotted in the other sub-tabs) and a <strong>dashed line</strong> for the median (the 50th percentile). When mean and median diverge, the distribution is skewed &mdash; typically the mean is pulled above the median by a long upper tail.",
        paste0(
          "The shaded band shows where <strong>80% of outcomes</strong> fall at each allocation. ",
          "Two band types are available in the sidebar:<br>",
          "&bull; <strong>Percentile (10&ndash;90%)</strong> &mdash; the symmetric cut between the 10th and 90th percentiles.<br>",
          "&bull; <strong>HDI (80%)</strong> &mdash; the narrowest interval containing 80% of outcomes. ",
          "For skewed distributions, HDI shifts toward where the data is densest, which often means a tighter band centered closer to the mode."
        ),
        "For symmetric distributions, percentile and HDI bands coincide. For skewed distributions, HDI is the more honest summary of &ldquo;where most outcomes actually concentrate&rdquo; &mdash; the percentile version can include sparse upper-tail regions to maintain symmetry.",
        "<strong>Tip:</strong> Where the rebalanced and non-rebalanced bands overlap, the colors combine into a third shade. Toggle one strategy off in the sidebar to see the other&rsquo;s band more clearly."
      ))
    } else {
      callout_bullets(c(
        "Each point is a <strong>point estimate</strong> (mean gain and mean SD) from 5,000 simulations at one stock/bond allocation.",
        "For the underlying distributions these points are derived from, click the <strong>Means + clouds</strong> sub-tab above.",
        "Two curves: one rebalanced, one non-rebalanced. Where rebalancing shifts the curve upward or leftward, it&rsquo;s improving the risk-return tradeoff on average.",
        "Use <strong>Zoom to frontier</strong> in the sidebar to toggle between a tight view around the frontier and the full axis range. You can also drag a box on the plot to zoom to a custom region."
      ))
    }
  })
}
