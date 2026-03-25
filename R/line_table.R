# ── line_table() ───────────────────────────────────────────────────────────────
#
# Builds one treatment panel containing three linked elements:
#
#   1. Subject-page selector  — crosstalk filter_select on the SUBJ_PAGE column.
#                               Lives inside the panel div (not global).
#   2. Plotly line chart      — one line per subject, click-to-highlight via
#                               crosstalk. The VSTEST filter driven by the global
#                               JavaScript also acts on this same SharedData group.
#   3. DT datatable           — linked to the same SharedData group: highlighting
#                               a subject in the plot filters the table rows too.
#
# Compound KEY (SUBJID_VSTESTCD) is used as the crosstalk row key so that the
# global VSTEST JavaScript filter can show/hide individual parameter lines while
# keeping each treatment's highlight group fully independent.
#
# The function receives data that is already restricted to one treatment arm.
# All other pre-processing (KEY, SUBJ_PAGE columns) is done in app.R.


#' Build a linked subject-page selector + plotly chart + DT table panel
#'
#' @param data       Data frame pre-filtered to a single TRTA value.
#'                   Must contain KEY and SUBJ_PAGE columns (from add_display_columns()).
#' @param trt        Character. Treatment label — used as the panel title and as
#'                   the crosstalk group name.
#' @param line_color Character. Hex colour for lines and markers.
#' @return An htmltools div forming one self-contained treatment panel.
line_table <- function(data, trt, line_color) {

  safe_trt <- gsub("[^A-Za-z0-9]", "_", trt)
  tmp_data <- SharedData$new(data, key = ~KEY, group = trt)

  # ── 1. Subject-page filter (per-panel, crosstalk) ───────────────────────────
  # Placed inside the panel div so each treatment has its own selector.
  # Filters by SUBJ_PAGE; crosstalk intersects this with the global VSTEST
  # filter automatically (both target the same group).
  page_filter <- filter_select(
    id         = paste0("page_", safe_trt),
    label      = "Subjects (page)",
    sharedData = tmp_data,
    ~SUBJ_PAGE,
    multiple   = FALSE   # single-page selection; empty = show all subjects
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
    style    = "bootstrap",
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
      page_filter
    ),

    tmp_plot,
    div(style = "margin-top: 12px;", tmp_table)
  )
}
