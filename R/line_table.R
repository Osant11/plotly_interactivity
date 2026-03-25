# ── line_table() ───────────────────────────────────────────────────────────────
# Renders one treatment panel: an interactive plotly line chart linked via
# crosstalk to a DT datatable.  Clicking a subject line in the chart dims all
# other subjects in both the chart and the table (crosstalk highlight).
#
# The function expects *pre-filtered* data (one treatment, one parameter).
# All upstream filtering (parameter, treatment, subject page) is handled by
# app.R before this function is called.


#' Build a linked plotly + DT panel for one treatment arm
#'
#' @param data       Data frame pre-filtered to a single TRTA value.
#' @param trt        Character. Treatment label (used for the panel title and
#'                   as the crosstalk group key).
#' @param line_color Character. Hex colour for the lines and markers.
#' @return An htmltools div containing the plotly widget and the DT widget.
line_table <- function(data, trt, line_color) {

  # SharedData links the plot and the table within this panel.
  # Each treatment gets its own group so highlights stay independent.
  tmp_data <- SharedData$new(data, key = ~SUBJID, group = trt)

  # ── Plot ────────────────────────────────────────────────────────────────────
  tmp_plot <- plot_ly(
    data          = tmp_data,
    x             = ~ADY,
    y             = ~CHG,
    color         = I(line_color),
    hovertemplate = paste0(
      "<b>%{customdata}</b><br>",
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

  # ── Table ───────────────────────────────────────────────────────────────────
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

  # ── Combined panel ──────────────────────────────────────────────────────────
  div(
    style = "flex: 1; min-width: 400px;",
    tmp_plot,
    div(style = "margin-top: 12px;", tmp_table)
  )
}
