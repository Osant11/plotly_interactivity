library(dplyr)
library(plotly)
library(crosstalk)
library(DT)
library(htmltools)
library(jsonlite)

source("R/filters.R")
source("R/line_table.R")
source("R/ui_components.R")

TRT_COLORS <- c(
  "Drug A 10mg" = "#dc2626",
  "Drug A 20mg" = "#2563eb",
  "Placebo"     = "#16a34a"
)

vs_display     <- add_display_columns(readRDS("data/vs.rds"))
all_treatments <- sort(unique(vs_display$TRTA))
all_vstests    <- sort(unique(vs_display$VSTEST))

all_groups <- unlist(lapply(all_treatments, function(trt) {
  pages <- sort(unique(vs_display$SUBJ_PAGE[vs_display$TRTA == trt]))
  c(paste0(trt, "_All"), paste0(trt, "_p", pages))
}))

panels <- lapply(all_treatments, function(trt)
  line_table(dplyr::filter(vs_display, TRTA == trt), trt, TRT_COLORS[[trt]])
)

browsable(tagList(

  tags$div(
    style = "display:flex; align-items:center; gap:28px; flex-wrap:wrap; padding:12px 18px; margin-bottom:18px; background:#ffffff; border:1px solid #e2e8f0; border-radius:8px;",
    build_global_parameter_control(all_vstests),
    tags$div(style = "width:1px; height:26px; background:#e2e8f0; align-self:center; flex-shrink:0;"),
    build_global_treatment_control(all_treatments, TRT_COLORS)
  ),

  tags$div(style = "display:flex; flex-wrap:wrap; gap:18px;", panels),

  build_filter_js(build_vstest_key_map(vs_display), all_groups, all_treatments,
                  all_vstests[1], build_chg_range(vs_display))
))
