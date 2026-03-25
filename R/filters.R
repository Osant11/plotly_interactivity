# ── Data helpers for display filtering ────────────────────────────────────────
#
# Two columns are pre-computed and attached to the dataset before any widget
# is built.  All filtering in the page relies on them:
#
#   KEY       = paste0(SUBJID, "_", VSTESTCD)
#               Compound crosstalk row key (subject × parameter).
#               Allows the JavaScript VSTEST filter to restrict individual
#               parameter lines while keeping per-treatment crosstalk groups
#               independent (needed for isolated click-highlighting).
#
#   SUBJ_PAGE = global range label based on SUBJID rank, e.g. "1–10", "11–20".
#               Used by the per-panel crosstalk filter_select for subject paging.
#
# Page size is defined once here so it is shared with app.R.

SUBJECT_PAGE_SIZE <- 10L


#' Add KEY and SUBJ_PAGE columns to the VS dataset
#'
#' @param data      Full VS data frame (must contain SUBJID and VSTESTCD).
#' @param page_size Subjects per page (default SUBJECT_PAGE_SIZE).
#' @return data with two new columns: KEY (character) and SUBJ_PAGE (character).
add_display_columns <- function(data, page_size = SUBJECT_PAGE_SIZE) {
  all_subj <- sort(unique(data$SUBJID))
  n        <- length(all_subj)

  # Build a SUBJID → SUBJ_PAGE mapping frame
  page_map <- do.call(rbind, lapply(seq(1L, n, by = page_size), function(s) {
    e <- min(s + page_size - 1L, n)
    data.frame(
      SUBJID    = all_subj[s:e],
      SUBJ_PAGE = paste0(all_subj[s], "\u2013", all_subj[e]),
      stringsAsFactors = FALSE
    )
  }))

  data |>
    dplyr::mutate(KEY = paste0(SUBJID, "_", VSTESTCD)) |>
    dplyr::left_join(page_map, by = "SUBJID")
}


#' Build a VSTEST → KEY-vector mapping for the JavaScript filter
#'
#' The list is serialised to JSON and embedded in the page as a lookup table.
#' When the user changes the parameter dropdown, the JS reads this table and
#' calls FilterHandle.set() on every treatment group simultaneously.
#'
#' @param data  Data frame after add_display_columns() (needs KEY and VSTEST).
#' @return Named list: VSTEST label → character vector of matching KEY strings.
build_vstest_key_map <- function(data) {
  vstests <- sort(unique(data$VSTEST))
  stats::setNames(
    lapply(vstests, function(vt) unique(data$KEY[data$VSTEST == vt])),
    vstests
  )
}
