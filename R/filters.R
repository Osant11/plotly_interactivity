# ── Data filtering functions ───────────────────────────────────────────────────
# Each function is a pure transformation: it takes a data frame and returns a
# filtered data frame. All higher-level orchestration lives in app.R.


#' Filter dataset by VSTEST (parameter)
#'
#' @param data   Data frame with a VSTEST column.
#' @param vstest Single VSTEST value to keep, or "All" / NULL to keep everything.
#' @return Filtered data frame.
filter_by_parameter <- function(data, vstest) {
  if (is.null(vstest) || vstest == "All") return(data)
  dplyr::filter(data, VSTEST %in% vstest)
}


#' Filter dataset by treatment arm(s) (TRTA)
#'
#' @param data       Data frame with a TRTA column.
#' @param treatments Character vector of treatments to keep.
#'                   NULL or zero-length keeps everything.
#' @return Filtered data frame.
filter_by_treatment <- function(data, treatments) {
  if (is.null(treatments) || length(treatments) == 0) return(data)
  dplyr::filter(data, TRTA %in% treatments)
}


#' Return the sorted unique subject IDs present in a dataset
#'
#' @param data Data frame with a numeric SUBJID column.
#' @return Sorted numeric vector of unique SUBJID values.
get_ordered_subjects <- function(data) {
  sort(unique(data$SUBJID))
}


#' Build page-range labels for a selectInput
#'
#' Given a sorted vector of subject IDs and a page size, returns a named
#' character vector of the form c("All", "1-10", "11-20", ...) suitable for
#' use as `choices` in selectInput().
#'
#' @param subjects  Sorted numeric vector from get_ordered_subjects().
#' @param page_size Integer. Subjects per page (default 10).
#' @return Named character vector of page labels (values == labels).
build_page_choices <- function(subjects, page_size = 10) {
  n <- length(subjects)
  if (n == 0) return(c("All" = "All"))

  labels <- "All"
  starts <- seq(1, n, by = page_size)
  for (s in starts) {
    e     <- min(s + page_size - 1, n)
    labels <- c(labels, paste0(subjects[s], "\u2013", subjects[e]))
  }
  labels
}


#' Restrict a dataset to a single subject page
#'
#' @param data      Data frame with a SUBJID column.
#' @param page      Page label produced by build_page_choices() (e.g. "1\u201310").
#'                  "All" or NULL returns the full dataset.
#' @param page_size Must match the value used in build_page_choices().
#' @return Filtered data frame.
filter_by_subject_page <- function(data, page, page_size = 10) {
  if (is.null(page) || page == "All") return(data)

  subjects <- get_ordered_subjects(data)
  n        <- length(subjects)
  starts   <- seq(1, n, by = page_size)

  for (s in starts) {
    e     <- min(s + page_size - 1, n)
    label <- paste0(subjects[s], "\u2013", subjects[e])
    if (identical(page, label)) {
      return(dplyr::filter(data, SUBJID %in% subjects[s:e]))
    }
  }

  data  # fallback: return everything if label not matched
}
