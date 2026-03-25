line_table <- function(sd, trt, line_color) {
  # Each treatment gets its own group → fully independent crosstalk
  tmp_data <- SharedData$new(filter(sd, TRTA == trt), key = ~SUBJID, group = trt)
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
    customdata    = ~SUBJID,
    height        = 600
  ) |>
    
    add_trace(     split         = ~SUBJID,
                   type          = "scatter",
                   mode          = "lines",
                   line          = list(width = 1.5), 
                   showlegend    = FALSE ) %>% 
    
    add_trace( type   = "scatter", 
               mode = "markers", 
               symbol = ~TYPET, 
               symbols = c('circle','x','o'),
               marker = list(size = 10)) %>% 
    layout(
      title  = list(
        text = paste0("<b>", trt, "</b>"),
        font = list(size = 15, color = "#1e293b", family = "Georgia, serif")
      ),
      xaxis  = list(title = "Visit",   tickfont = list(size = 11, color = "#64748b"),
                    gridcolor = "#f1f5f9", linecolor = "#e2e8f0"),
      yaxis  = list(title = "CHG",     tickfont = list(size = 11, color = "#64748b"),
                    gridcolor = "#f1f5f9", linecolor = "#e2e8f0", zeroline = FALSE),
      legend = list(text = "Assessment Type:", 
                    orientation = "h", 
                    x = 0.5, 
                    xanchored = "centered", 
                    y = -0.2 ),
      plot_bgcolor  = "#fafafa",
      paper_bgcolor = "#ffffff",
      font = list(family = "Georgia, serif")
    ) |>
    config(displayModeBar = FALSE)
  
  
  tmp_plot <- highlight(
    tmp_plot,
    on         = "plotly_click",
    off        = "plotly_doubleclick",
    opacityDim = 0.08,
    selected   = attrs_selected(line = list(width = 3), marker = list(size = 9))
  )
  # Table using the same SharedData object
  
  tmp_table <- datatable(
    tmp_data,
    style    = "bootstrap",
    width    = "100%",
    rownames = FALSE,
    options  = list(
      scrollX = TRUE, 
      pageLength = 5,
      dom        = "tip"  # t = table, i = info, p = pagination
    )
  )
  # Combine plot + table into one self-contained div
  div(
    style = "flex: 1; min-width: 400px;",
    tmp_plot,
    div(style = "margin-top: 12px;", tmp_table)
  )
}