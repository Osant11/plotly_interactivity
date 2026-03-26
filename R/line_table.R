# ── line_table() ───────────────────────────────────────────────────────────────
#
# Builds one treatment panel containing three linked elements:
#
#   1. Subject-page selector  — plain HTML <select> (NOT crosstalk filter_select).
#                               Lives inside the panel div (not global).
#                               Calls filterBySubjectPage() defined in
#                               ui_components.R, which drives its own
#                               FilterHandle so that "All subjects" (value = "")
#                               correctly calls FilterHandle.clear().
#                               (filter_select with multiple=FALSE cannot be
#                               cleared back to "all" in selectize single-select
#                               mode, which is why we use a plain select here.)
#   2. Plotly line chart      — one line per subject, click-to-highlight via
#                               crosstalk. The global VSTEST filter and the
#                               per-panel subject-page filter both target this
#                               same SharedData group; crosstalk ANDs them.
#   3. DT datatable           — linked to the same SharedData group.
#
# Compound KEY (SUBJID_VSTESTCD) is used as the crosstalk row key so that the
# global VSTEST JavaScript filter can restrict individual parameter lines while
# keeping each treatment's highlight group fully independent.
#
# The function receives data already restricted to one treatment arm.
# All pre-processing (KEY, SUBJ_PAGE columns) is done in app.R.


#' Build a linked subject-page selector + plotly chart + DT table panel
#'
#' @param data       Data frame pre-filtered to a single TRTA value.
#'                   Must contain KEY and SUBJ_PAGE columns (from add_display_columns()).
#' @param trt        Character. Treatment label — used as the panel title and as
#'                   the crosstalk group name.
#' @param line_color Character. Hex colour for lines and markers.
#' @return An htmltools div forming one self-contained treatment panel.
line_table <- function(data, trt, line_color) {

  tmp_data <- SharedData$new(data, key = ~KEY, group = trt)

  # ── 1. Subject-page selector (per-panel, plain HTML <select>) ───────────────
  # Plain HTML is used instead of crosstalk's filter_select because selectize's
  # single-select mode cannot be cleared back to "nothing selected" once a value
  # has been chosen.  Our custom JS function filterBySubjectPage() explicitly
  # calls FilterHandle.clear() when the "All subjects" option is selected.
  js_trt      <- gsub("'", "\\'", trt, fixed = TRUE)   # safe for JS string
  page_values <- sort(unique(data$SUBJ_PAGE))

  page_select <- tags$div(
    style = "display: flex; align-items: center; gap: 8px;",
    tags$label(
      style = paste0(
        "font-weight: bold; font-size: 13px; white-space: nowrap; ",
        "font-family: Georgia, serif; color: #1e293b;"
      ),
      "Subjects (page)"
    ),
    tags$select(
      onchange = paste0("filterBySubjectPage('", js_trt, "', this.value)"),
      style    = paste0(
        "padding: 4px 8px; border: 1px solid #cbd5e1; border-radius: 4px; ",
        "font-family: Georgia, serif; font-size: 13px; color: #1e293b; ",
        "background: #fff; cursor: pointer;"
      ),
      # Empty value → FilterHandle.clear() → all subjects restored
      tags$option(value = "", "All subjects"),
      lapply(page_values, function(pg) tags$option(value = pg, pg))
    )
  )

  # ── 2. Plotly line chart ────────────────────────────────────────────────────
  tmp_plot <- plot_ly(
    data          = tmp_data,
    x             = ~ADY,
    y             = ~CHG,
    color         = I(line_color),
    hovertemplate = paste0(
      "<b>Subject %{customdata}</b><br>",
      "Visit: %{x}<br>",
      "CHG: %{y}<extra></extra>"
    ),
    customdata = ~SUBJID,
    height     = 500
  ) |>
    add_trace(
      split      = ~SUBJID,
      type       = "scatter",
      mode       = "lines",
      line       = list(width = 1.5),
      showlegend = FALSE
    ) |>
    add_trace(
      type    = "scatter",
      mode    = "markers",
      symbol  = ~TYPET,
      symbols = c("circle", "x", "o"),
      marker  = list(size = 10)
    ) |>
    layout(
      title = list(
        text = paste0("<b>", trt, "</b>"),
        font = list(size = 15, color = "#1e293b", family = "Georgia, serif")
      ),
      xaxis = list(
        title     = "Visit",
        tickfont  = list(size = 11, color = "#64748b"),
        gridcolor = "#f1f5f9",
        linecolor = "#e2e8f0"
      ),
      yaxis = list(
        title     = "CHG",
        tickfont  = list(size = 11, color = "#64748b"),
        gridcolor = "#f1f5f9",
        linecolor = "#e2e8f0",
        zeroline  = FALSE
      ),
      legend = list(
        orientation = "h",
        x           = 0.5,
        xanchor     = "center",
        y           = -0.2
      ),
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff",
      font          = list(family = "Georgia, serif")
    ) |>
    config(displayModeBar = FALSE)

  tmp_plot <- highlight(
    tmp_plot,
    on         = "plotly_click",
    off        = "plotly_doubleclick",
    opacityDim = 0.08,
    selected   = attrs_selected(line = list(width = 3), marker = list(size = 9))
  )

  # ── 3. DT datatable ─────────────────────────────────────────────────────────
  tmp_table <- datatable(
    tmp_data,
    style    = "default",
    width    = "100%",
    rownames = FALSE,
    options  = list(
      scrollX    = TRUE,
      pageLength = 5,
      dom        = "tip"   # table + info + pagination; no search box
    )
  )

  # ── Panel div ────────────────────────────────────────────────────────────────
  # data-treatment is read by toggleTreatmentPanels() in ui_components.R.
  # class="treatment-panel" is the CSS target for show/hide.
  div(
    class            = "treatment-panel",
    `data-treatment` = trt,
    style            = paste0(
      "flex: 1; min-width: 400px; display: flex; flex-direction: column; ",
      "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; ",
      "padding: 14px; box-sizing: border-box;"
    ),

    # Subject-page selector at the top of the panel
    div(
      style = paste0(
        "margin-bottom: 10px; padding: 8px 10px; ",
        "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 5px;"
      ),
      page_select
    ),

    tmp_plot,
    div(style = "margin-top: 12px;", tmp_table)
  )
}
