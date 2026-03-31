build_plot_table <- function(sd, trt, line_color, table_id) {
  p <- plot_ly(
    data          = sd,
    x             = ~ADY,
    y             = ~CHG,
    color         = I(line_color),
    hovertemplate = "<b>Subject %{customdata}</b><br>Visit: %{x}<br>CHG: %{y}<extra></extra>",
    customdata    = ~SUBJID,
    height        = 500
  ) |>
    add_trace(split = ~SUBJID, type = "scatter", mode = "lines",
              line = list(width = 1.5), showlegend = FALSE) |>
    add_trace(type = "scatter", mode = "markers",
              symbol = ~TYPET, symbols = c("circle", "x", "o"),
              marker = list(size = 10)) |>
    layout(
      title  = list(text = paste0("<b>", trt, "</b>"),
                    font = list(size = 15, color = "#1e293b", family = "Georgia, serif")),
      xaxis  = list(title = "Visit", tickfont = list(size = 11, color = "#64748b"),
                    gridcolor = "#f1f5f9", linecolor = "#e2e8f0"),
      yaxis  = list(title = "CHG",   tickfont = list(size = 11, color = "#64748b"),
                    gridcolor = "#f1f5f9", linecolor = "#e2e8f0", zeroline = FALSE),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2),
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff",
      font          = list(family = "Georgia, serif")
    ) |>
    config(displayModeBar = FALSE) |>
    highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.08,
              selected = attrs_selected(line = list(width = 3), marker = list(size = 9)))

  tagList(
    p,
    div(style = "margin-top: 12px;",
        datatable(sd, elementId = table_id, style = "default", width = "100%",
                  rownames = FALSE, options = list(scrollX = TRUE, pageLength = 5, dom = "tip")))
  )
}

line_table <- function(data, trt, line_color) {
  safe_trt <- gsub("[^A-Za-z0-9]", "_", trt)
  pages    <- sort(unique(data$SUBJ_PAGE))

  make_sd <- function(d, suffix)
    SharedData$new(d, key = ~KEY, group = paste0(trt, "_", suffix))

  sd_all   <- make_sd(data, "All")
  sd_pages <- stats::setNames(
    lapply(pages, function(pg) make_sd(data[data$SUBJ_PAGE == pg, ], paste0("p", pg))),
    as.character(pages)
  )

  select_id <- paste0("page_sel_", safe_trt)
  page_select <- tags$div(
    style = "margin-bottom:10px; padding:8px 10px; background:#f8fafc; border:1px solid #e2e8f0; border-radius:5px; display:inline-flex; align-items:center; gap:10px;",
    tags$label(`for` = select_id,
               style = "font-weight:bold; font-size:13px; font-family:Georgia,serif; color:#1e293b;",
               "Subjects (page)"),
    tags$select(
      id = select_id,
      onchange = sprintf("switchPage('%s', this.value)", safe_trt),
      style = "padding:4px 8px; border:1px solid #cbd5e1; border-radius:5px; font-family:Georgia,serif; font-size:13px; color:#1e293b; background:#fff;",
      c(list(tags$option(value = "All", "All")),
        lapply(pages, function(pg) tags$option(value = pg, paste0("Page ", pg))))
    )
  )

  make_page_div <- function(sd, key) {
    div(class = paste0("page-view page-view-", safe_trt), `data-page` = key,
        style = "display:block;",
        build_plot_table(sd, trt, line_color, paste0("dt_", safe_trt, "_", key)))
  }

  div(
    class = "treatment-panel", `data-treatment` = trt,
    style = "flex:1; min-width:400px; display:flex; flex-direction:column; background:#ffffff; border:1px solid #e2e8f0; border-radius:8px; padding:14px; box-sizing:border-box;",
    page_select,
    make_page_div(sd_all, "All"),
    lapply(as.character(pages), function(pg) make_page_div(sd_pages[[pg]], pg))
  )
}
