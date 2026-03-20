# server.R — Shiny server logic

function(input, output, session) {

  # Reactive values to store results
  sim_result <- reactiveVal(NULL)
  sweep_result <- reactiveVal(NULL)

  # --- Run Single Simulation ---
  observeEvent(input$run_single, {
    shinyjs::disable("run_single")
    shinyjs::disable("run_sweep")

    # Convert percentage to fraction, clamping to avoid 0/1 exactly
    perc <- max(0.000001, min(0.999999, input$perc_stocks / 100))

    withProgress(message = "Running simulation...", value = 0, {
      set.seed(100)
      result <- run_simulation(
        n = input$n_timesteps,
        s.mean = input$s_mean,
        s.sd = input$s_sd,
        b.mean = input$b_mean,
        b.sd = input$b_sd,
        s.b.corr = input$s_b_corr,
        init.inv = input$init_inv,
        perc.stocks = perc,
        rebal.interval = input$rebal_interval,
        s.int = input$s_int,
        b.int = input$b_int,
        int.period = input$int_period,
        propStepDivisor = input$propStepDivisor,
        nSims = input$nSims,
        nSimsToRecord = input$nSimsToRecord,
        widthVal = input$widthVal,
        progress_callback = function(i, total, msg) {
          setProgress(value = i / total, message = msg)
        }
      )
      sim_result(result)
    })

    shinyjs::enable("run_single")
    shinyjs::enable("run_sweep")
  })

  # --- Run Frontier Sweep ---
  observeEvent(input$run_sweep, {
    shinyjs::disable("run_single")
    shinyjs::disable("run_sweep")

    # Parse comma-separated allocation percentages
    alloc_pcts <- as.numeric(trimws(unlist(strsplit(input$sweep_allocs, ","))))
    alloc_pcts <- alloc_pcts[!is.na(alloc_pcts)]
    # Convert to fractions, clamp extremes
    alloc_fracs <- pmax(0.000001, pmin(0.999999, alloc_pcts / 100))

    total_work <- length(alloc_fracs) * input$nSims

    withProgress(message = "Running frontier sweep...", value = 0, {
      set.seed(100)
      result <- run_meta_sweep(
        perc.stocks.vec = alloc_fracs,
        n = input$n_timesteps,
        s.mean = input$s_mean,
        s.sd = input$s_sd,
        b.mean = input$b_mean,
        b.sd = input$b_sd,
        s.b.corr = input$s_b_corr,
        init.inv = input$init_inv,
        rebal.interval = input$rebal_interval,
        s.int = input$s_int,
        b.int = input$b_int,
        int.period = input$int_period,
        propStepDivisor = input$propStepDivisor,
        nSims = input$nSims,
        nSimsToRecord = input$nSimsToRecord,
        widthVal = input$widthVal,
        progress_callback = function(alloc_i, total_allocs, sim_i, total_sims, msg) {
          done <- (alloc_i - 1) * total_sims + sim_i
          setProgress(value = done / total_work, message = msg)
        }
      )
      sweep_result(result)
    })

    # Auto-switch to Frontier tab
    updateTabsetPanel(session, "main_tabs", selected = "Efficient Frontier")

    shinyjs::enable("run_single")
    shinyjs::enable("run_sweep")
  })

  # --- Plot outputs ---

  output$plot_gain_sd <- renderPlot({
    req(sim_result())
    plot_gain_vs_sd(sim_result())
  })

  output$plot_trajectories <- renderPlot({
    req(sim_result())
    plot_trajectories(sim_result(), max_sims = min(100, sim_result()$params$nSims))
  })

  output$plot_costs <- renderPlot({
    req(sim_result())
    plot_cost_profiles(sim_result(), max_sims = min(10, sim_result()$params$nSims))
  })

  output$plot_densities <- renderPlot({
    req(sim_result())
    plot_final_densities(sim_result())
  })

  output$plot_delta <- renderPlot({
    req(sim_result())
    plot_rebal_delta(sim_result())
  })

  output$plot_explorer <- renderPlot({
    req(sim_result())
    simSel <- input$simSel
    plot_single_sim_explorer(sim_result(), simSel)
  })

  output$plot_frontier <- renderPlot({
    req(sweep_result())
    plot_efficient_frontier(sweep_result())
  })

  # --- Summary table ---

  output$summary_table <- renderTable({
    req(sim_result())
    df <- sim_result()$gainsVsSD.point.estDf
    df$gain <- paste0(round(df$gain * 100, 3), "%")
    df$sd <- round(df$sd, 2)
    names(df) <- c("Std Dev", "Annualized Gain", "Strategy")
    df
  })

  output$params_text <- renderPrint({
    req(sim_result())
    p <- sim_result()$params
    cat("Timesteps:", p$n, "\n")
    cat("Stock Mean:", p$s.mean, "| SD:", p$s.sd, "\n")
    cat("Bond Mean:", p$b.mean, "| SD:", p$b.sd, "\n")
    cat("Correlation:", p$s.b.corr, "\n")
    cat("Initial Investment: $", p$init.inv, "\n")
    cat("Stock Allocation:", round(p$perc.stocks * 100, 1), "%\n")
    cat("Rebalance Interval:", p$rebal.interval, "timesteps\n")
    cat("Stock Interest:", p$s.int, "| Bond Interest:", p$b.int, "\n")
    cat("Interest Period:", p$int.period, "timesteps\n")
    cat("Simulations:", p$nSims, "(", p$nSimsToRecord, "recorded)\n")
    cat("Rolling Window:", p$widthVal, "\n")
    cat("Proposal Step Divisor:", p$propStepDivisor, "\n")
  })
}
