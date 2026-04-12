# server.R — Shiny server logic

function(input, output, session) {

  # Hardcoded parameters (not exposed in UI)
  s.mean <- 10
  b.mean <- 10
  init.inv <- 1000
  propStepDivisor <- 20
  widthVal <- 600
  int.period <- 300

  # Screenshot mode: set to a sim number to highlight it on the Gain vs SD plot.
  # Set to NULL for normal operation. Set screenshot_hide_cloud to TRUE to show
  # only point estimates (for "point estimate only" screenshot).
  screenshot_highlight_sim <- NULL
  screenshot_hide_cloud <- FALSE

  # Reactive values to store results
  sim_result <- reactiveVal(NULL)
  sweep_result <- reactiveVal(NULL)
  frontier_alloc_index <- reactiveVal(1)
  active_scenario <- reactiveVal("corr070_rebal100")  # tracks which precomputed scenario is loaded
  scenario_grid_open <- reactiveVal(FALSE)
  scenario_pinned <- reactiveVal(FALSE)

  # Helper: collapse the scenario grid
  collapse_grid <- function() {
    shinyjs::hide("scenario_grid_panel", anim = TRUE, animType = "slide", time = 0.2)
    shinyjs::hide("scenario_pin", anim = TRUE, animType = "fade", time = 0.15)
    shinyjs::removeClass("scenario_toggle", "open")
    scenario_grid_open(FALSE)
  }

  # Toggle scenario grid visibility
  observeEvent(input$scenario_toggle, {
    if (scenario_grid_open()) {
      collapse_grid()
      scenario_pinned(FALSE)
      shinyjs::removeClass("scenario_pin", "pinned")
    } else {
      shinyjs::show("scenario_grid_panel", anim = TRUE, animType = "slide", time = 0.2)
      shinyjs::show("scenario_pin", anim = TRUE, animType = "fade", time = 0.15)
      shinyjs::addClass("scenario_toggle", "open")
      scenario_grid_open(TRUE)
    }
  })

  # Pin/unpin scenario grid
  observeEvent(input$scenario_pin, {
    pinned <- !scenario_pinned()
    scenario_pinned(pinned)
    shinyjs::toggleClass("scenario_pin", "pinned")
    if (!pinned) collapse_grid()
  })
  # Cloud alpha is driven by the UI slider (input$cloud_alpha)
  # Highlight the label when cloud is fully hidden
  observe({
    alpha <- input$cloud_alpha
    req(!is.null(alpha))
    if (alpha == 0) {
      shinyjs::runjs("$('#cloud_alpha').closest('.form-group').addClass('cloud-hidden-warning')")
    } else {
      shinyjs::runjs("$('#cloud_alpha').closest('.form-group').removeClass('cloud-hidden-warning')")
    }
  })

  # Update cloud opacity in-place (no re-render, preserves zoom)
  observeEvent(input$cloud_alpha, {
    alpha <- input$cloud_alpha
    plotlyProxy("plot_gain_sd", session) %>%
      plotlyProxyInvoke("restyle", list(`marker.opacity` = alpha), list(0L, 1L))
    plotlyProxy("plot_frontier_explorer", session) %>%
      plotlyProxyInvoke("restyle", list(`marker.opacity` = alpha), list(0L, 1L))
  }, ignoreInit = TRUE)

  # Data source tracking: "precomputed" or "custom"
  single_source <- reactiveVal("precomputed")
  sweep_source <- reactiveVal("precomputed")

  # --- Auto-load precomputed data on startup ---
  observe({
    single_path <- "data/precomputed/single_n200.rds"
    frontier_path <- "data/precomputed/frontier_corr070_rebal100_n5000.rds"

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
      n_allocs <- length(unique(data$point_estimates$allocation))
      if (frontier_alloc_index() > n_allocs) frontier_alloc_index(1)
      if (!scenario_pinned()) collapse_grid()
    }
  }

  # Scenario grid button observers
  scenarios <- expand.grid(
    corr = c("070", "030", "000", "n030"),
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

  # nSim alt buttons (500 / 2,000 at default scenario)
  observeEvent(input$nsim_500, {
    fname <- "data/precomputed/frontier_n500.rds"
    if (file.exists(fname)) {
      data <- readRDS(fname)
      sweep_result(data)
      sweep_source("precomputed")
      active_scenario("corr070_rebal100_n500")
      n_allocs <- length(unique(data$point_estimates$allocation))
      if (frontier_alloc_index() > n_allocs) frontier_alloc_index(1)
      if (!scenario_pinned()) collapse_grid()
    }
  })
  observeEvent(input$nsim_2000, {
    fname <- "data/precomputed/frontier_n2000.rds"
    if (file.exists(fname)) {
      data <- readRDS(fname)
      sweep_result(data)
      sweep_source("precomputed")
      active_scenario("corr070_rebal100_n2000")
      n_allocs <- length(unique(data$point_estimates$allocation))
      if (frontier_alloc_index() > n_allocs) frontier_alloc_index(1)
      if (!scenario_pinned()) collapse_grid()
    }
  })

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
    # nSim alt buttons
    for (n in c("500", "2000")) {
      btn_id <- paste0("nsim_", n)
      if (endsWith(current, paste0("_n", n))) {
        shinyjs::addClass(btn_id, "nsim-alt-active")
      } else {
        shinyjs::removeClass(btn_id, "nsim-alt-active")
      }
    }
  })

  # --- Restore precomputed data ---
  observeEvent(input$restore_precomputed, {
    single_path <- "data/precomputed/single_n200.rds"
    frontier_path <- "data/precomputed/frontier_corr070_rebal100_n5000.rds"

    if (file.exists(single_path)) {
      sim_result(readRDS(single_path))
      single_source("precomputed")
    }
    if (file.exists(frontier_path)) {
      sweep_result(readRDS(frontier_path))
      sweep_source("precomputed")
      active_scenario("corr070_rebal100")
      frontier_alloc_index(1)
    }
  })

  # --- Reset all sliders to defaults ---
  observeEvent(input$reset_defaults, {
    updateSliderInput(session, "nSims", value = 500)
    updateSliderInput(session, "n_timesteps", value = 1000)
    updateSliderInput(session, "perc_stocks", value = 60)
    updateSliderInput(session, "rebal_interval", value = 100)
    updateSliderInput(session, "cloud_alpha", value = 0.08)
    updateSliderInput(session, "s_sd", value = 6)
    updateSliderInput(session, "b_sd", value = 3)
    updateSliderInput(session, "s_b_corr", value = 0.7)
    updateSliderInput(session, "s_int", value = 0.06)
    updateSliderInput(session, "b_int", value = 0.02)
  })

  # --- Status banner ---
  output$data_status_banner <- renderUI({
    tab <- input$main_tabs

    if (tab == "Single Allocation") {
      src <- single_source()
      res <- sim_result()
      if (is.null(res)) return(NULL)
      nsims <- res$params$nSims
      if (src == "precomputed") {
        msg <- paste0("Showing precomputed results (", nsims,
                       " sims, 60/40 allocation). ",
                       "Adjust parameters and run your own, or switch to Frontier Sweep ",
                       "to explore precomputed scenarios.")
      } else {
        msg <- paste0("Showing custom simulation results (", nsims, " sims).")
      }
    } else if (tab == "Frontier Sweep") {
      src <- sweep_source()
      sw <- sweep_result()
      if (is.null(sw)) return(NULL)
      nsims <- nrow(sw$cloud) / length(unique(sw$cloud$allocation)) / 2
      if (src == "precomputed") {
        sc <- active_scenario()
        if (grepl("^corr", sc)) {
          base <- sub("_n\\d+$", "", sc)
          parts <- regmatches(base, regexec("corr(n?\\d+)_rebal(\\d+)", base))[[1]]
          corr_val <- if (substr(parts[2], 1, 1) == "n") {
            paste0("\u2212", as.numeric(sub("n", "", parts[2])) / 100)
          } else {
            as.character(as.numeric(parts[2]) / 100)
          }
          nsim_str <- if (grepl("_n\\d+$", sc)) {
            format(as.numeric(sub(".*_n", "", sc)), big.mark = ",")
          } else {
            "5,000"
          }
          msg <- paste0("Showing precomputed frontier (", nsim_str,
                         " sims, correlation ", corr_val,
                         ", rebalance every ", parts[3], " days).")
        } else {
          msg <- paste0("Showing precomputed frontier (",
                         format(round(nsims), big.mark = ","),
                         " sims per allocation).")
        }
      } else {
        msg <- paste0("Showing custom frontier sweep results (",
                       format(round(nsims), big.mark = ","), " sims per allocation).")
      }
    } else {
      return(NULL)
    }

    div(class = "data-status-banner", msg)
  })

  # --- Disable perc_stocks and update button label when tab changes ---
  observe({
    if (input$main_tabs == "Frontier Sweep") {
      shinyjs::disable("perc_stocks")
      updateActionButton(session, "run_single", label = "Run Frontier Sweep")
    } else {
      shinyjs::enable("perc_stocks")
      updateActionButton(session, "run_single", label = "Run Single Allocation")
    }
  })

  # --- Run button: show confirmation modal ---
  observeEvent(input$run_single, {
    is_frontier <- input$main_tabs == "Frontier Sweep"
    nsims <- input$nSims

    if (is_frontier) {
      n_allocs <- 11
      total_sims <- n_allocs * nsims
      # Rough time estimate: ~0.1s per sim on typical hardware
      est_minutes <- round(total_sims * 0.1 / 60, 1)
      title <- "Run Frontier Sweep?"
      body <- tagList(
        p(paste0("This will run ", format(total_sims, big.mark = ","),
                 " simulations (", n_allocs, " allocations \u00d7 ",
                 format(nsims, big.mark = ","), " sims each).")),
        p(tags$strong(paste0("Estimated time: ~", est_minutes, " minutes")),
          " (varies by hardware; frontier sweeps at 1,000+ sims can take much longer)."),
        p("This will replace the current frontier data. ",
          "You can restore precomputed results anytime.")
      )
    } else {
      est_seconds <- round(nsims * 0.1)
      title <- "Run Single Allocation?"
      body <- tagList(
        p(paste0("This will run ", format(nsims, big.mark = ","),
                 " simulations at ", input$perc_stocks, "% stocks / ",
                 100 - input$perc_stocks, "% bonds.")),
        p(tags$strong(paste0("Estimated time: ~", est_seconds, " seconds")),
          " (varies by hardware)."),
        p("This will replace the current single-allocation data. ",
          "You can restore precomputed results anytime.")
      )
    }

    showModal(modalDialog(
      title = title,
      body,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_run", "Run", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # --- Confirmed run ---
  observeEvent(input$confirm_run, {
    removeModal()
    shinyjs::disable("run_single")

    perc <- clamp_pct_to_fraction(input$perc_stocks)
    nSimsToRecord <- min(10, input$nSims)

    is_frontier <- input$main_tabs == "Frontier Sweep"

    if (is_frontier) {
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
          widthVal = widthVal,
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
    } else {
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
          widthVal = widthVal,
          progress_callback = function(i, total, msg) {
            setProgress(value = i / total, message = msg)
          }
        )
        sim_result(result)
        single_source("custom")
      })
    }

    shinyjs::enable("run_single")
  })

  # --- Plotly config: download + reset axes ---
  plotly_clean <- function(p) {
    config(p, displayModeBar = TRUE,
           modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d",
                                       "zoomIn2d", "zoomOut2d", "autoScale2d",
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
      cloud_alpha_override = isolate(input$cloud_alpha))))
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

  # --- Frontier plots (plotly) ---

  output$plot_frontier <- renderPlotly({
    req(sweep_result())
    plotly_clean(ggplotly(plot_efficient_frontier(sweep_result()$summary)))
  })

  # --- Frontier Explorer ---

  # Store zoom state so it persists across allocation steps
  frontier_zoom <- reactiveValues(x = NULL, y = NULL)

  # Capture zoom/pan events from plotly
  observe({
    req(sweep_result())
    ev <- event_data("plotly_relayout", source = "frontier_src")
    req(ev)
    # Zoom sets xaxis.range[0]/[1] and yaxis.range[0]/[1]
    if (!is.null(ev[["xaxis.range[0]"]])) {
      frontier_zoom$x <- c(ev[["xaxis.range[0]"]], ev[["xaxis.range[1]"]])
      frontier_zoom$y <- c(ev[["yaxis.range[0]"]], ev[["yaxis.range[1]"]])
    }
    # Double-click or reset axes fires autosize or xaxis.autorange
    if (isTRUE(ev[["xaxis.autorange"]]) || !is.null(ev[["autosize"]])) {
      frontier_zoom$x <- NULL
      frontier_zoom$y <- NULL
    }
  })

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

  output$plot_frontier_explorer <- renderPlotly({
    req(sweep_result())
    p <- plotly_clean(ggplotly(
      plot_frontier_explorer(sweep_result(), frontier_alloc_index(),
                             show_cloud = TRUE,
                             cloud_alpha_override = isolate(input$cloud_alpha)),
      source = "frontier_src"
    ) %>% event_register("plotly_relayout"))

    # Reapply saved zoom if present
    if (!is.null(frontier_zoom$x)) {
      p <- p %>% layout(
        xaxis = list(range = frontier_zoom$x),
        yaxis = list(range = frontier_zoom$y)
      )
    }
    p
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

  output$callout_frontier_explorer <- renderUI({
    req(sweep_result())
    if (sweep_source() != "precomputed") return(NULL)
    callout(
      "The frontier sweep runs the full MCMC simulation at each stock/bond allocation
       (0% to 100% in 10% steps). The <strong>highlighted cloud</strong> shows the full
       distribution of outcomes for the selected allocation &mdash; every dot is one
       simulation. The <strong>connected points</strong> along the frontier lines are
       point estimates (means), which trace the efficient frontier. The gap between the
       rebalanced and non-rebalanced frontiers is the rebalancing benefit.
       See the <strong>Efficient Frontier</strong> tab for just the point estimates
       (from a 5,000-sim run). You can zoom in by dragging a box on the plot;
       the zoom is preserved as you step through allocations. Use the
       <strong>Cloud Opacity</strong> slider to fade or hide the cloud."
    )
  })

  output$callout_frontier <- renderUI({
    req(sweep_result())
    if (sweep_source() != "precomputed") return(NULL)
    callout(
      "The efficient frontier: each point is a <strong>point estimate</strong>
       (mean gain and mean SD across all simulations) for one stock/bond allocation.
       These are from a precomputed run of 5,000 simulations per allocation.
       These are summary statistics &mdash; the Frontier Explorer tab shows the full
       distributions from which they&rsquo;re derived. Two curves are shown: one for the
       rebalanced strategy and one for non-rebalanced. Where rebalancing shifts the
       curve upward or leftward, it&rsquo;s improving the risk-return tradeoff."
    )
  })
}
