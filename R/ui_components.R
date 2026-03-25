# ── UI component builders ──────────────────────────────────────────────────────
# Each function returns a self-contained Shiny UI element.
# Input IDs are declared as constants at the top so they are shared cleanly
# between ui_components.R and app.R without duplication.


# ── Input ID constants ─────────────────────────────────────────────────────────
ID_PARAMETER    <- "parameter"
ID_TREATMENTS   <- "treatments"
ID_SUBJECT_PAGE <- "subject_page"


#' Dropdown to select the parameter to display (based on VSTEST)
#'
#' @param all_parameters Character vector of all unique VSTEST values.
#' @return A selectInput shiny tag.
build_parameter_selector <- function(all_parameters) {
  selectInput(
    inputId  = ID_PARAMETER,
    label    = tags$span(
      tags$b("Parameter"),
      tags$small(" (VSTEST)", style = "color:#64748b;")
    ),
    choices  = c("All" = "All", setNames(all_parameters, all_parameters)),
    selected = all_parameters[1]
  )
}


#' Checkbox group to select which treatment arm(s) to display
#'
#' All treatments are pre-selected. Unchecking one hides that panel.
#'
#' @param all_treatments Character vector of all unique TRTA values.
#' @return A checkboxGroupInput shiny tag.
build_treatment_selector <- function(all_treatments) {
  checkboxGroupInput(
    inputId  = ID_TREATMENTS,
    label    = tags$b("Treatments"),
    choices  = all_treatments,
    selected = all_treatments   # all visible by default
  )
}


#' Dropdown to page through subjects within the treatment panels
#'
#' The choices are populated dynamically by the server via updateSelectInput();
#' only "All" is set here as the initial placeholder.
#'
#' @return A selectInput shiny tag.
build_subject_page_selector <- function() {
  selectInput(
    inputId  = ID_SUBJECT_PAGE,
    label    = tags$span(
      tags$b("Subjects"),
      tags$small(" (page)", style = "color:#64748b;")
    ),
    choices  = "All",
    selected = "All"
  )
}
