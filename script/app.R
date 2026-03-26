# ── VS Vital Signs Explorer ────────────────────────────────────────────────────
#
# Static interactive HTML — no Shiny required.
# Run the script and the page opens in RStudio's viewer (or the default browser).
#
# Three filter controls
# ──────────────────────────────────────────────────────────────────────────────
#  GLOBAL (rendered above all panels)
#   1. Parameter selector  HTML <select> → only distinct VSTEST values.
#                          Broadcasts to all treatment groups via a crosstalk
#                          FilterHandle, so only the chosen parameter's lines
#                          and table rows are visible across every panel.
#
#   2. Treatment selector  HTML checkboxes (all pre-ticked).
#                          Unchecking a treatment hides that entire panel div
#                          using CSS display toggling (no data is destroyed).
#
#  PER-PANEL (inside each treatment div, above the plot)
#   3. Subject page        plain HTML <select> calling filterBySubjectPage() in JS.
#                          Shows subjects 10 at a time (e.g. "1–10", "11–20").
#                          "All subjects" explicitly calls FilterHandle.clear() so
#                          the filter is fully removed.  crosstalk ANDs this with
#                          filter (1) automatically via a separate FilterHandle.
#
# File structure
# ──────────────────────────────────────────────────────────────────────────────
#  app.R                  ← this file: data prep + page assembly
#  R/filters.R            ← add_display_columns(), build_vstest_key_map()
#  R/line_table.R         ← per-panel plot + table + subject-page selector
#  R/ui_components.R      ← global HTML controls + JS bridge functions
#  data/vs.rds            ← pre-built dataset (run data/dummy_vs.R to rebuild)

library(dplyr)
library(plotly)
library(crosstalk)
library(DT)
library(htmltools)
library(jsonlite)

source("R/filters.R")
source("R/line_table.R")
source("R/ui_components.R")

# ── Constants ──────────────────────────────────────────────────────────────────

TRT_COLORS <- c(
  "Drug A 10mg" = "#dc2626",
  "Drug A 20mg" = "#2563eb",
  "Placebo"     = "#16a34a"
)

# ── 1. Load and pre-process data ───────────────────────────────────────────────
# add_display_columns() attaches KEY (compound crosstalk key) and SUBJ_PAGE
# (global subject-range label) to every row.

vs            <- readRDS("data/vs.rds")
vs_display    <- add_display_columns(vs, page_size = SUBJECT_PAGE_SIZE)

all_treatments <- sort(unique(vs_display$TRTA))
all_vstests    <- sort(unique(vs_display$VSTEST))   # no "All" — distinct only
default_vstest <- all_vstests[1]                     # activated on page load

vstest_key_map <- build_vstest_key_map(vs_display)  # VSTEST → KEYs (for JS)
subj_page_map  <- build_subj_page_map(vs_display)   # treatment → page → KEYs
chg_ranges     <- build_chg_range(vs_display)        # VSTEST → shared y-axis range

# ── 2. Build per-treatment panels ─────────────────────────────────────────────
# Each panel is self-contained: its own SharedData group, subject-page selector,
# plotly chart, and DT table.

panels <- lapply(all_treatments, function(trt) {
  trt_data <- dplyr::filter(vs_display, TRTA == trt)
  line_table(trt_data, trt, TRT_COLORS[[trt]])
})

# ── 3. Assemble and display the page ──────────────────────────────────────────

browsable(
  tagList(

    # ── Global controls bar ──────────────────────────────────────────────────
    tags$div(
      style = paste0(
        "display: flex; align-items: center; gap: 28px; flex-wrap: wrap; ",
        "padding: 12px 18px; margin-bottom: 18px; ",
        "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px;"
      ),

      build_global_parameter_control(all_vstests),

      # Thin vertical divider
      tags$div(style = paste0(
        "width: 1px; height: 26px; background: #e2e8f0; ",
        "align-self: center; flex-shrink: 0;"
      )),

      build_global_treatment_control(all_treatments, TRT_COLORS)
    ),

    # ── Treatment panels ─────────────────────────────────────────────────────
    tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 18px;",
      panels
    ),

    # ── JavaScript for all three filter behaviours ───────────────────────────
    build_filter_js(vstest_key_map, subj_page_map, all_treatments, default_vstest, chg_ranges)
  )
)
