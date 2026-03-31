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
#   SUBJ_PAGE = integer page number within each treatment arm (1, 2, 3 …).
#               Computed by add_display_columns() using within-group rank so
#               page 1 of each treatment always starts at its first subject.
#               build_subject_page_data() then produces a second copy of the
#               data where every row has SUBJ_PAGE = "All", bound together
#               with the paged version so that the per-panel filter_select
#               has an explicit "All" option alongside the page numbers.
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


#' Compute global CHG y-axis range per VSTEST across all treatments
#'
#' @param data  Full VS data frame after add_display_columns() (needs VSTEST, CHG).
#' @param pad   Fractional padding added to each side of the range (default 0.05).
#' @return Named list: VSTEST label → list(min = <num>, max = <num>).
build_chg_range <- function(data, pad = 0.05) {
  vstests <- sort(unique(data$VSTEST))
  stats::setNames(
    lapply(vstests, function(vt) {
      vals <- data$CHG[data$VSTEST == vt]
      vals <- vals[!is.na(vals)]
      rng  <- diff(range(vals))
      list(
        min = floor(min(vals)   - pad * rng),
        max = ceiling(max(vals) + pad * rng)
      )
    }),
    vstests
  )
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


#' Build the duplicated dataset used by the per-panel subject-page filter_select
#'
#' The trick that makes "All subjects" work inside a crosstalk filter_select:
#' each subject row is duplicated — once with its real SUBJ_PAGE number and
#' once with SUBJ_PAGE = "All".  The "All" copy is prepended so it appears
#' first in the filter_select dropdown.
#'
#' The filter_select widget uses this duplicated data (same crosstalk group as
#' the plot/table).  Selecting "All" activates every KEY in the group (all
#' subjects visible); selecting a page number activates only that page's KEYs.
#' Switching back to "All" now picks a real dropdown value instead of trying
#' to clear a selectize single-select — which was the root cause of the bug.
#'
#' @param data  A single-treatment data frame after add_display_columns()
#'              (needs KEY and SUBJ_PAGE).
#' @return data frame with original rows (SUBJ_PAGE as character "1", "2" …)
#'         plus one copy per row with SUBJ_PAGE = "All", "All" rows first.
build_subject_page_data <- function(data) {
  paged <- dplyr::mutate(data, SUBJ_PAGE = as.character(SUBJ_PAGE))
  dplyr::bind_rows(
    dplyr::mutate(paged, SUBJ_PAGE = "All"),   # "All" first → appears first in dropdown
    paged
  )
}
