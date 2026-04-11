# Monte Carlo Portfolio Simulator — UI Redesign

## Context

I have a working R Shiny app deployed at:
  https://ssickels.shinyapps.io/r-shiny-portfolio/

It needs to be:
1. Embedded in a new page on my site (stevessite.com) via iframe
2. The Shiny app itself needs a UI redesign (two-tab layout, parameter hierarchy)
3. An About page needs to be added to the site

My site is hosted on GoDaddy cPanel (Linux). It uses a dark background / light text
theme (theme.css), a hamburger nav injected by nav.js, and a consistent page
structure. See existing page examples below.

---

## Deliverable 1 — Site page: monte-carlo.html

Create a new page at /monte-carlo/index.html (or adjust path to match site structure).

Requirements:
- Load theme.css and nav.js the same way other site pages do
- Page header: title "Monte Carlo Portfolio Simulator" + one-sentence description
- A light-background wrapper div (#f7f7f5) containing the iframe — Shiny's ggplot
  output is white/light, so this prevents it jarring against the dark site chrome
- iframe src: https://ssickels.shinyapps.io/r-shiny-portfolio/
- iframe height: calc(100vh - 160px), min-height 600px
- Small note below iframe: "Simulation runs on a remote R server. Initial load and
  longer runs (especially the frontier sweep) may take a few minutes."
- Add a "Monte Carlo Simulator" section to nav.js with links to the simulator
  page and the about page

---

## Deliverable 2 — About page: monte-carlo-about.html

A complete about page is provided as a reference file (monte-carlo-about.html).
Integrate it into the site using the same header/nav pattern as other pages.
The page includes an inline SVG diagram — preserve it exactly.

One correction to apply to the SVG: the two frontier curves must meet at both
endpoints (at 100% bonds and 100% stocks, there is nothing to rebalance, so
the strategies are identical). Both paths should start and end at the same
coordinates, diverging only in the middle.

---

## Deliverable 3 — Shiny app UI redesign

This is the main work. The current app has a flat list of tabs and gives equal
visual weight to all parameters. The redesign has two modes and a cleaner
parameter hierarchy.

### Tab structure

Replace the current tab layout with two top-level mode tabs:

**Tab 1: Single allocation**
Answers: "Given a fixed stock/bond split, what does the distribution of outcomes
look like — and does rebalancing help?"

Sub-tabs (in this order):
1. Gain vs SD — hero plot, show first
2. Final densities
3. Trajectories
4. Cost profiles
5. Single sim explorer

**Tab 2: Frontier sweep**
Answers: "Across all allocations, which mix offers the best risk/return tradeoff?"

Sub-tabs:
1. Frontier explorer (the 11-step allocation stepper)
2. Efficient frontier

Show a prominent warning on the Efficient Frontier sub-tab:
"This sweep runs a full simulation for each of 11 allocations. Expect several
minutes at n=500. At n=100 the frontier shape is noisy; n=1000+ gives a clean
curve."

### Sidebar parameter hierarchy

**Always visible — "What you're testing":**
- Number of simulations (n) — slider, 100–1500, step 100
- Time horizon (steps) — slider
- Stock allocation (%) — slider 0–100, step 10
  (gray out / disable in Frontier sweep mode, since the sweep covers all allocations)
- Rebalance interval — slider
- Initial investment ($) — numeric input

**Collapsed by default — "Market assumptions":**
Use Shiny's bsCollapse or similar to hide these until expanded:
- Stock mean price
- Stock std dev
- Bond mean price
- Bond std dev
- Stock–bond correlation
- Stock interest rate
- Bond interest rate

### Run button
A prominent "Run simulation" button at the bottom of the sidebar.
Use withProgress() / incProgress() inside the simulation loop so the user
sees "Iteration X of N" rather than a frozen screen.

### Notes on the current code
- The simulation loop already has print() statements at each iteration —
  replace these with incProgress() calls
- Calculations and plots are already modular (separate functions) —
  the Shiny wrapper should call these functions, not inline the logic
- eventReactive(input$run_btn, {...}) should wrap the simulation so it only
  runs on button click, not on every parameter change

---

## Reference files

- monte-carlo-about.html — complete about page (use as-is, integrate nav/theme)
- monte-carlo-mockup.html — visual mockup of the simulator embed page layout
- monte-carlo-ui-mockup.html — interactive mockup of the two-tab Shiny UI concept

---

## Site conventions (from existing pages)

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <title>Page Title — Steve's Site</title>
  <link rel="stylesheet" href="../theme.css">
  <style>
    body { overflow-y: auto; }
    .page-wrap {
      max-width: 740px;       /* use 1100px for full-width iframe pages */
      margin: 0 auto;
      padding: 72px var(--space-lg) var(--space-xl);
    }
  </style>
</head>
<body>
  <script src="../nav.js"></script>
  <div class="page-wrap">
    <!-- content here -->
  </div>
</body>
</html>
```

nav.js self-injects — no placeholder div needed.
CSS variables in use: --space-sm, --space-md, --space-lg, --space-xl,
--color-border, --color-text-dim, --color-accent (teal ~#1D9E75).
