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

SUBJECT_PAGE_SIZE <- 2L

#' Add KEY and SUBJ_PAGE columns to the VS dataset
#'
#' @param data      Full VS data frame (must contain SUBJID and VSTESTCD).
#' @param page_size Subjects per page (default SUBJECT_PAGE_SIZE).
#' @return data with two new columns: KEY (character) and SUBJ_PAGE (character).
add_display_columns <- function(data, arg_grp = "TRTA", page_size = SUBJECT_PAGE_SIZE) {
 
  # Build SUBJID → SUBJ_PAGE mapping within each treatment group
  page_map <- data |>
    dplyr::select(SUBJID, !!rlang::sym(arg_grp)) |>
    dplyr::distinct() |>
    dplyr::arrange(!!rlang::sym(arg_grp), SUBJID) |>
    dplyr::group_by(!!rlang::sym(arg_grp)) |>
    dplyr::mutate(
      row_in_grp = dplyr::row_number(),
      page_num   = ceiling(row_in_grp / page_size)
    ) |>
    dplyr::group_by(!!rlang::sym(arg_grp), page_num) |>
    dplyr::mutate(
            SUBJ_PAGE = page_num
      #SUBJ_PAGE = paste0(dplyr::first(row_in_grp), "\u2013", dplyr::last(row_in_grp))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(SUBJID, !!rlang::sym(arg_grp), SUBJ_PAGE)
 
  data |>
    dplyr::mutate(KEY = paste0(SUBJID, "_", VSTESTCD)) |>
    dplyr::left_join(page_map, by = c("SUBJID", arg_grp))
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
