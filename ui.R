# ui.R — Shiny UI definition

# Dev flag: set FALSE to hide layer-toggles disclosure in sidebar for production
dev_layer_toggles <- TRUE

fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  # ===== Full-width tab navigation =====
  div(class = "main-tab-nav",
    tabsetPanel(
      id = "main_tabs",
      tabPanel(
        title = span(class = "tab-label-wrapper",
          span("Single allocation", class = "tab-main-label"),
          span("Explore one stock/bond split", class = "tab-sub-label")
        ),
        value = "Single allocation"
      ),
      tabPanel(
        title = span(class = "tab-label-wrapper",
          span("Frontier", class = "tab-main-label"),
          span("See how outcomes change across mixes", class = "tab-sub-label")
        ),
        value = "Frontier"
      )
    )
  ),

  # ===== Sidebar + Main content =====
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # ===== FRONTIER MODE: Scenario grid (always visible) =====
      conditionalPanel("input.main_tabs == 'Frontier'",
        h4("Frontier Selector"),
        div(class = "sidebar-section-scenarios",
          div(class = "scenario-grid-label",
            "Select correlation & rebalance interval"),
          tags$table(class = "scenario-grid",
            tags$tr(
              tags$th(""),
              tags$th(colspan = "3",
                      style = "text-align:center; font-weight:400; padding-bottom:1px;",
                      "Rebalance interval")
            ),
            tags$tr(
              tags$th("Corr", style = "text-align:right; padding-right:6px;"),
              tags$th("25"),
              tags$th("100"),
              tags$th("300")
            ),
            tags$tr(
              tags$th("0.7", style = "text-align:right; padding-right:6px;"),
              tags$td(actionButton("scenario_070_25", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_070_100", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_070_300", "\u2022", class = "btn-sm scenario-btn"))
            ),
            tags$tr(
              tags$th("0.3", style = "text-align:right; padding-right:6px;"),
              tags$td(actionButton("scenario_030_25", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_030_100", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_030_300", "\u2022", class = "btn-sm scenario-btn"))
            ),
            tags$tr(
              tags$th("0.0", style = "text-align:right; padding-right:6px;"),
              tags$td(actionButton("scenario_000_25", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_000_100", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_000_300", "\u2022", class = "btn-sm scenario-btn"))
            ),
            tags$tr(
              tags$th("\u22120.3", style = "text-align:right; padding-right:6px;"),
              tags$td(actionButton("scenario_n030_25", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_n030_100", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_n030_300", "\u2022", class = "btn-sm scenario-btn"))
            ),
            tags$tr(
              tags$th("\u22120.7", style = "text-align:right; padding-right:6px;"),
              tags$td(actionButton("scenario_n070_25", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_n070_100", "\u2022", class = "btn-sm scenario-btn")),
              tags$td(actionButton("scenario_n070_300", "\u2022", class = "btn-sm scenario-btn"))
            )
          ),
        ),
        hr()
      ),

      # ===== FRONTIER MODE: Display section =====
      conditionalPanel("input.main_tabs == 'Frontier'",
        h4("Display"),
        checkboxInput("zoom_to_frontier", "Zoom to frontier", value = TRUE),
        actionButton("reset_view", "Reset view", class = "btn-sm reset-view-btn", style = "display:none;"),
        conditionalPanel("input.frontier_tabs == 'clouds'",
          div(style = "margin-top: 12px;",
            sliderInput("cloud_alpha_frontier", "Cloud Opacity",
                        min = 0, max = 0.30, value = 0.08, step = 0.01)
          ),
          if (dev_layer_toggles) tags$details(class = "layer-toggles",
            tags$summary(
              style = "cursor:pointer; color:#6b5b4f; font-size:15px; font-weight:500;",
              "Layer toggles"
            ),
            div(style = "padding-top:4px;",
              checkboxInput("layer_cloud_rebal", "Cloud (rebal)", TRUE),
              checkboxInput("layer_cloud_nonrebal", "Cloud (nonrebal)", TRUE),
              sliderInput("cloud_sample_n", "Cloud points",
                          min = 500, max = 5000, value = 5000, step = 500),
              checkboxInput("layer_line_rebal", "Frontier line (rebal)", TRUE),
              checkboxInput("layer_line_nonrebal", "Frontier line (nonrebal)", TRUE),
              sliderInput("frontier_line_width", "Line width",
                          min = 0.5, max = 5, value = 0.5, step = 0.5),
              sliderInput("frontier_line_alpha", "Line opacity",
                          min = 0, max = 1, value = 0.4, step = 0.1),
              checkboxInput("layer_dots_rebal", "Dots (rebal)", TRUE),
              checkboxInput("layer_dots_nonrebal", "Dots (nonrebal)", TRUE),
              checkboxInput("layer_highlight_current", "Highlight current", TRUE),
              checkboxInput("layer_dots_others", "Other allocations", TRUE),
              sliderInput("dot_size", "Dot size",
                          min = 1, max = 6, value = 2, step = 0.5),
              checkboxInput("layer_labels", "Allocation labels", FALSE),
              checkboxInput("sim_show_rebal", "Sim plot (rebal)", TRUE),
              checkboxInput("sim_show_nonrebal", "Sim plot (nonrebal)", TRUE),
              actionButton("reset_layer_toggles", "Reset layers",
                           class = "btn-sm btn-default")
            )
          )
        )
      ),

      conditionalPanel("input.main_tabs == 'Frontier'", hr()),

      # ===== SHARED PARAMS: <details> wrapper =====
      # In Single allocation mode: forced open, summary acts as static heading
      # In Frontier mode: collapsible "Simulation" heading
      tags$details(id = "params_details", open = NA,
        tags$summary(id = "params_summary",
          tags$span(class = "params-heading", "Simulation")
        ),
        div(id = "params_content", style = "padding-top:4px;",
          # Frontier mode: warning + run button at top of expander
          conditionalPanel("input.main_tabs == 'Frontier'",
            p(class = "rerun-warning",
              "Running a frontier sweep is slow \u2014 it simulates every allocation ",
              "from 0% to 100% stocks. At high sim counts this can take several minutes ",
              "and may time out on free-tier hosting."),
            actionButton("run_frontier", "Run frontier sweep",
                          class = "btn-default btn-sm"),
            hr()
          ),

          sliderInput("nSims", "Number of Simulations",
                      min = 100, max = 1500, value = 500, step = 100),
          sliderInput("n_timesteps", "Time Horizon (days)",
                      min = 250, max = 2000, value = 1000, step = 250),

          hr(),

          h4("Portfolio"),
          # Stock allocation only relevant in single mode
          conditionalPanel("input.main_tabs == 'Single allocation'",
            sliderInput("perc_stocks", "Stock Allocation (%)",
                        min = 0, max = 100, value = 60, step = 10)
          ),
          sliderInput("rebal_interval", "Rebalance Interval",
                      min = 10, max = 500, value = 100, step = 10),

          hr(),

          # --- Market Assumptions (collapsible) ---
          tags$details(
            tags$summary(
              style = "cursor:pointer; color:#6b5b4f; font-size:15px; font-weight:500;",
              "Market assumptions"
            ),
            div(style = "padding-top:8px;",
              sliderInput("s_sd", "Stock Volatility (SD)",
                          min = 1, max = 15, value = 6, step = 0.5),
              sliderInput("b_sd", "Bond Volatility (SD)",
                          min = 1, max = 15, value = 3, step = 0.5),
              sliderInput("s_b_corr", "Stock-Bond Correlation",
                          min = -1, max = 1, value = 0.7, step = 0.05),
              sliderInput("s_int", "Stock Interest Rate",
                          min = 0, max = 0.2, value = 0.06, step = 0.005),
              sliderInput("b_int", "Bond Interest Rate",
                          min = 0, max = 0.2, value = 0.02, step = 0.005)
            )
          )
        )
      ),

      # ===== SINGLE MODE: Display section (cloud alpha, Gain vs Volatility only) =====
      conditionalPanel(
        "input.main_tabs == 'Single allocation' && input.single_tabs == 'Gain vs Volatility'",
        hr(),
        h4("Display"),
        sliderInput("cloud_alpha_single", "Cloud Opacity",
                    min = 0, max = 0.30, value = 0.08, step = 0.01)
      ),

      # ===== SINGLE MODE: Run button =====
      conditionalPanel("input.main_tabs == 'Single allocation'",
        hr(),
        actionButton("run_single", "Run Single Allocation",
                      class = "btn-primary btn-block")
      ),

      # ===== Bottom actions =====
      div(class = "sidebar-bottom-actions",
        actionButton("restore_precomputed", "Restore precomputed data",
                      class = "btn-default btn-sm sidebar-action-btn"),
        actionButton("reset_defaults", "Reset to defaults",
                      class = "btn-default btn-sm sidebar-action-btn")
      )
    ),

    mainPanel(
      width = 9,

      # --- Single allocation content ---
      conditionalPanel("input.main_tabs == 'Single allocation'",
        uiOutput("data_status_banner"),
        tabsetPanel(
          id = "single_tabs", type = "pills",
          tabPanel(title = span("Gain vs Volatility",
                     img(src = "single_gain_vs_sd.png", class = "pill-thumb")),
                   value = "Gain vs Volatility",
            plotlyOutput("plot_gain_sd", height = "600px"),
            uiOutput("callout_gain_sd")
          ),
          tabPanel(title = span("Single Sim Explorer",
                     img(src = "single_explorer.png", class = "pill-thumb")),
                   value = "Single Sim Explorer",
            div(class = "sim-explorer-nav",
              actionButton("sim_prev", label = NULL,
                           icon = icon("arrow-left"), class = "btn-sm"),
              tags$span(class = "sim-label",
                        textOutput("sim_explorer_label", inline = TRUE)),
              actionButton("sim_next", label = NULL,
                           icon = icon("arrow-right"), class = "btn-sm")
            ),
            plotlyOutput("plot_explorer", height = "800px"),
            uiOutput("callout_explorer")
          ),
          tabPanel("Summary",
                   h4("Point Estimates"),
                   tableOutput("summary_table"),
                   hr(),
                   h4("Simulation Parameters"),
                   verbatimTextOutput("params_text")
          )
        )
      ),

      # --- Frontier content ---
      conditionalPanel("input.main_tabs == 'Frontier'",
        # Sub-tab selector (mode switch, not content container)
        tabsetPanel(id = "frontier_tabs", type = "pills",
          tabPanel(title = span("Just the means",
                     img(src = "frontier_no_cloud.png", class = "pill-thumb")),
                   value = "means"),
          tabPanel(title = span("Means + clouds",
                     img(src = "frontier_cloud.png", class = "pill-thumb")),
                   value = "clouds")
        ),
        # Shared frontier plot
        plotlyOutput("plot_frontier_merged", height = "600px"),
        # Allocation navigator + highlight (clouds mode only)
        conditionalPanel("input.frontier_tabs == 'clouds'",
          div(class = "frontier-alloc-nav",
            actionButton("frontier_prev", label = NULL,
                         icon = icon("arrow-left"), class = "btn-sm"),
            tags$strong(class = "frontier-alloc-label",
                        textOutput("frontier_alloc_label", inline = TRUE)),
            actionButton("frontier_next", label = NULL,
                         icon = icon("arrow-right"), class = "btn-sm")
          ),
          div(class = "frontier-highlight-row",
            actionButton("highlight_random_sim", "Highlight a simulation",
                         class = "btn-sm btn-default"),
            actionButton("clear_highlight", "Clear highlight",
                         class = "btn-sm btn-link",
                         style = "display:none;")
          )
        ),
        div(id = "frontier_sim_explorer_panel", style = "display:none;",
          plotlyOutput("plot_frontier_sim_values", height = "300px"),
          div(class = "plot-callout frontier-sim-caption",
            "This is the actual simulated history behind the highlighted pair above. ",
            "The two strategies share the same underlying price paths \u2014 they differ ",
            "only in whether the portfolio is periodically rebalanced to the target allocation."
          ),
          tags$details(class = "frontier-sim-details",
            tags$summary(
              style = "cursor:pointer; color:#6b5b4f; font-size:15px; font-weight:500;",
              "See shares and prices"
            ),
            div(style = "padding-top:8px;",
              plotlyOutput("plot_frontier_sim_detail", height = "400px")
            )
          )
        ),
        uiOutput("callout_frontier_merged")
      )
    )
  )
)
