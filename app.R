# ── VS Vital Signs Explorer ────────────────────────────────────────────────────
# Interactive Shiny app with three filter controls:
#
#   1. Parameter selector  – choose which VSTEST (e.g. HEART RATE) to display.
#   2. Treatment selector  – choose which treatment arm(s) to show as panels.
#   3. Subject page        – page through subjects (1-10, 11-20 …) within the
#                            visible treatment panels for a cleaner view.
#
# File structure
# ├── app.R                 ← you are here (UI + server orchestration)
# ├── R/
# │   ├── filters.R         ← pure data-filtering functions
# │   ├── line_table.R      ← plotly + DT panel builder (one treatment)
# │   └── ui_components.R   ← Shiny UI control builders + input ID constants
# └── data/
#     ├── dummy_vs.R        ← data-generation script
#     └── vs.rds            ← pre-built dataset

library(shiny)
library(plotly)
library(crosstalk)
library(DT)
library(dplyr)
library(htmltools)

source("R/filters.R")
source("R/line_table.R")
source("R/ui_components.R")

# ── Constants ──────────────────────────────────────────────────────────────────

vs <- readRDS("data/vs.rds")

ALL_TREATMENTS    <- sort(unique(vs$TRTA))
ALL_PARAMETERS    <- sort(unique(vs$VSTEST))
SUBJECT_PAGE_SIZE <- 10

TRT_COLORS <- c(
  "Drug A 10mg" = "#dc2626",
  "Drug A 20mg" = "#2563eb",
  "Placebo"     = "#16a34a"
)

# ── UI ─────────────────────────────────────────────────────────────────────────

ui <- fluidPage(

  tags$head(tags$style(HTML("
    body          { font-family: Georgia, serif; background: #f8fafc; }
    .well         { background: #ffffff; border: 1px solid #e2e8f0;
                    border-radius: 8px; padding: 16px; }
    .sidebar-note { font-size: 12px; color: #94a3b8; margin-top: 4px; }
    h2            { color: #1e293b; font-size: 20px; margin-bottom: 16px; }
    hr            { border-color: #e2e8f0; }
  "))),

  titlePanel(
    tags$h2("VS Vital Signs Explorer")
  ),

  sidebarLayout(

    sidebarPanel(
      width = 3,

      # ── Control 1: Parameter ──────────────────────────────────────────────
      build_parameter_selector(ALL_PARAMETERS),

      hr(),

      # ── Control 2: Treatments ─────────────────────────────────────────────
      build_treatment_selector(ALL_TREATMENTS),
      tags$p("Uncheck to hide a panel.", class = "sidebar-note"),

      hr(),

      # ── Control 3: Subject page ───────────────────────────────────────────
      build_subject_page_selector(),
      tags$p(
        paste0("Shows ", SUBJECT_PAGE_SIZE, " subjects per page."),
        class = "sidebar-note"
      )
    ),

    mainPanel(
      width = 9,
      uiOutput("treatment_panels")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Step 1: Filter by parameter (VSTEST) ───────────────────────────────────
  # Applied globally; determines which rows are ever visible.
  parameter_data <- reactive({
    filter_by_parameter(vs, input[[ID_PARAMETER]])
  })

  # ── Step 2: Filter by selected treatment arms ──────────────────────────────
  # Determines which treatment panels are rendered.
  treatment_data <- reactive({
    filter_by_treatment(parameter_data(), input[[ID_TREATMENTS]])
  })

  # ── Step 3: Sync subject-page choices with current data ────────────────────
  # Rebuilds page labels whenever the parameter or treatment filter changes,
  # and resets the selector to "All" to avoid stale selections.
  observe({
    subjects     <- get_ordered_subjects(treatment_data())
    page_choices <- build_page_choices(subjects, SUBJECT_PAGE_SIZE)
    updateSelectInput(session, ID_SUBJECT_PAGE,
                      choices  = page_choices,
                      selected = "All")
  })

  # ── Step 4: Filter by subject page ────────────────────────────────────────
  # Applied only to the treatment panels (not to the parameter/treatment
  # filter steps above), giving a focused view without changing the data scope.
  display_data <- reactive({
    filter_by_subject_page(treatment_data(), input[[ID_SUBJECT_PAGE]], SUBJECT_PAGE_SIZE)
  })

  # ── Step 5: Render treatment panels ───────────────────────────────────────
  # One panel (plot + linked table) per selected treatment.
  output$treatment_panels <- renderUI({

    sel_trts <- input[[ID_TREATMENTS]]
    data     <- display_data()

    # Guard: nothing selected or no data after filters
    if (is.null(sel_trts) || length(sel_trts) == 0 || nrow(data) == 0) {
      return(div(
        style = "padding: 60px; text-align: center; color: #94a3b8;",
        tags$p(tags$em("No data available for the current selection."))
      ))
    }

    # Only keep treatments that actually have rows in the current data
    trts_to_show <- intersect(sel_trts, unique(data$TRTA))

    panels <- lapply(trts_to_show, function(trt) {
      trt_data <- dplyr::filter(data, TRTA == trt)
      line_table(trt_data, trt, TRT_COLORS[[trt]])
    })

    div(
      style = "display: flex; flex-wrap: wrap; gap: 20px; padding: 8px 0;",
      panels
    )
  })
}

# ── Launch ─────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
