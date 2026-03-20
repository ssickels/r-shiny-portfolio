# ui.R — Shiny UI definition

fluidPage(
  useShinyjs(),
  titlePanel("Monte Carlo Portfolio Simulator"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # --- Asset Distributions ---
      h4("Asset Distributions"),
      numericInput("s_mean", "Stock Mean Price", value = 10, min = 0.1, step = 0.5),
      numericInput("s_sd", "Stock Std Dev", value = 6, min = 0.1, step = 0.5),
      numericInput("b_mean", "Bond Mean Price", value = 10, min = 0.1, step = 0.5),
      numericInput("b_sd", "Bond Std Dev", value = 6, min = 0.1, step = 0.5),
      sliderInput("s_b_corr", "Stock-Bond Correlation", min = -1, max = 1,
                  value = 0.7, step = 0.05),

      hr(),

      # --- Portfolio Settings ---
      h4("Portfolio Settings"),
      numericInput("init_inv", "Initial Investment ($)", value = 1000, min = 1, step = 100),
      sliderInput("perc_stocks", "Stock Allocation (%)", min = 0, max = 100,
                  value = 60, step = 1),
      numericInput("rebal_interval", "Rebalance Interval (timesteps)", value = 100,
                   min = 1, step = 10),

      hr(),

      # --- Growth / Interest ---
      h4("Growth / Interest"),
      numericInput("s_int", "Stock Interest Rate", value = 0.04, min = 0, max = 1, step = 0.005),
      numericInput("b_int", "Bond Interest Rate", value = 0.04, min = 0, max = 1, step = 0.005),
      numericInput("int_period", "Interest Period (timesteps)", value = 300, min = 1, step = 50),

      hr(),

      # --- Simulation Settings ---
      h4("Simulation Settings"),
      numericInput("n_timesteps", "Number of Timesteps", value = 1000, min = 100, step = 100),
      numericInput("nSims", "Number of Simulations", value = 30, min = 1, step = 10),
      numericInput("nSimsToRecord", "Sims to Record (full trajectories)", value = 5,
                   min = 1, step = 1),
      numericInput("propStepDivisor", "Proposal Step Divisor", value = 20, min = 1, step = 1),
      numericInput("widthVal", "Rolling Window Width", value = 600, min = 10, step = 50),

      hr(),

      # --- Actions ---
      actionButton("run_single", "Run Simulation", class = "btn-primary btn-block"),

      hr(),

      # --- Frontier Sweep ---
      h4("Frontier Sweep"),
      textInput("sweep_allocs", "Allocations (comma-separated %)",
                value = "0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100"),
      actionButton("run_sweep", "Run Frontier Sweep", class = "btn-success btn-block"),

      hr(),

      # --- Sim Explorer ---
      h4("Sim Explorer"),
      numericInput("simSel", "Selected Simulation #", value = 1, min = 1, step = 1)
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Gain vs SD", plotOutput("plot_gain_sd", height = "600px")),
        tabPanel("Trajectories", plotOutput("plot_trajectories", height = "600px")),
        tabPanel("Cost Profiles", plotOutput("plot_costs", height = "600px")),
        tabPanel("Final Densities", plotOutput("plot_densities", height = "600px")),
        tabPanel("Rebal Delta", plotOutput("plot_delta", height = "600px")),
        tabPanel("Single Sim Explorer", plotOutput("plot_explorer", height = "800px")),
        tabPanel("Efficient Frontier", plotOutput("plot_frontier", height = "600px")),
        tabPanel("Summary",
                 h4("Point Estimates"),
                 tableOutput("summary_table"),
                 hr(),
                 h4("Simulation Parameters"),
                 verbatimTextOutput("params_text")
        )
      )
    )
  )
)
