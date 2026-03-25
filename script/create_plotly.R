library( haven )
library( dplyr )
library( tidyr )
library( stringr )
library( ggplot2 )
library( forcats )
library( plotly )
library( crosstalk )
library( DT )
library( htmltools )

vs <- readRDS( "data/vs.rds" )

treatments <- factor( unique( vs$TRTA ), levels = c( "Drug A 10mg", "Drug A 20mg", "Placebo" ) )

trt_colors <- c(
  "Drug A 10 mg" = "#dc2626",
  "Drug A 20 mg" = "#2563eb",
  "Placebo"        = "#16a34a"
)


facet_list <- mapply(
  FUN      = line_table,
  trt      = levels( treatments ),
  line_color = trt_colors[ treatments ],  # match color by name
  MoreArgs = list(sd = vs ),
  SIMPLIFY = FALSE
)


# Display side by side
browsable(
  div(
    style = "display: flex; flex-wrap: wrap; gap: 16px;",
    lapply(facet_list, function(p) {
      div(style = "flex: 1; min-width: 400px; height: 1000px;", p)
    })
  )
)
