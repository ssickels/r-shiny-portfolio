# ui.R — Shiny UI definition

fluidPage(
  useShinyjs(),
  tags$head(tags$link(rel = "stylesheet", href = "custom.css")),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # --- Simulation ---
      h4("Simulation"),
      sliderInput("nSims", "Number of Simulations",
                  min = 100, max = 1500, value = 500, step = 100),
      sliderInput("n_timesteps", "Time Horizon (days)",
                  min = 250, max = 2000, value = 1000, step = 250),

      hr(),

      # --- Portfolio ---
      h4("Portfolio"),
      sliderInput("perc_stocks", "Stock Allocation (%)",
                  min = 0, max = 100, value = 60, step = 10),
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
      ),

      hr(),

      # --- Actions ---
      actionButton("run_single", "Run Single Allocation", class = "btn-primary btn-block"),
      actionButton("restore_precomputed", "Restore precomputed data",
                    class = "btn-link btn-block restore-link")
    ),

    mainPanel(
      width = 9,

      # --- Status banner ---
      uiOutput("data_status_banner"),

      tabsetPanel(
        id = "main_tabs",

        # --- Single Allocation tab ---
        tabPanel("Single Allocation",
          tabsetPanel(
            id = "single_tabs", type = "pills",
            tabPanel("Gain vs SD",
              plotlyOutput("plot_gain_sd", height = "600px"),
              uiOutput("callout_gain_sd")
            ),
            tabPanel("Final Densities",
              plotlyOutput("plot_densities", height = "600px"),
              uiOutput("callout_densities")
            ),
            tabPanel("Trajectories",
              plotlyOutput("plot_trajectories", height = "600px"),
              uiOutput("callout_trajectories")
            ),
            tabPanel("Cost Profiles",
              plotlyOutput("plot_costs", height = "600px"),
              uiOutput("callout_costs")
            ),
            tabPanel("Single Sim Explorer",
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

        # --- Frontier Sweep tab ---
        tabPanel("Frontier Sweep",
          tabsetPanel(
            id = "frontier_tabs", type = "pills",
            tabPanel("Frontier Explorer",
              fluidRow(
                column(2, actionButton("frontier_prev", label = NULL,
                                       icon = icon("arrow-left"), class = "btn-sm")),
                column(8, align = "center",
                       tags$strong(textOutput("frontier_alloc_label", inline = TRUE))),
                column(2, align = "right",
                       actionButton("frontier_next", label = NULL,
                                    icon = icon("arrow-right"), class = "btn-sm"))
              ),
              fluidRow(
                column(12, align = "center", style = "margin-top:6px; margin-bottom:4px;",
                  tags$span(style = "font-size:14px; color:#6b5b4f; margin-right:8px;",
                            "Precomputed sims:"),
                  actionButton("nsim_500", "500",
                               class = "btn-sm", style = "margin:0 2px;"),
                  actionButton("nsim_2000", "2,000",
                               class = "btn-sm", style = "margin:0 2px;"),
                  actionButton("nsim_5000", "5,000",
                               class = "btn-sm", style = "margin:0 2px;")
                )
              ),
              plotlyOutput("plot_frontier_explorer", height = "600px"),
              uiOutput("callout_frontier_explorer")
            ),
            tabPanel("Efficient Frontier",
              plotlyOutput("plot_frontier", height = "600px"),
              uiOutput("callout_frontier")
            )
          )
        )
      )
    )
  )
)
