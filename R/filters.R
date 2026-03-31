SUBJECT_PAGE_SIZE <- 2L

add_display_columns <- function(data, arg_grp = "TRTA", page_size = SUBJECT_PAGE_SIZE) {
  grp <- rlang::sym(arg_grp)
  page_map <- data |>
    dplyr::select(SUBJID, !!grp) |>
    dplyr::distinct() |>
    dplyr::arrange(!!grp, SUBJID) |>
    dplyr::group_by(!!grp) |>
    dplyr::mutate(SUBJ_PAGE = ceiling(dplyr::row_number() / page_size)) |>
    dplyr::ungroup() |>
    dplyr::select(SUBJID, !!grp, SUBJ_PAGE)

  data |>
    dplyr::mutate(KEY = paste0(SUBJID, "_", VSTESTCD)) |>
    dplyr::left_join(page_map, by = c("SUBJID", arg_grp))
}

build_chg_range <- function(data, pad = 0.05) {
  vstests <- sort(unique(data$VSTEST))
  stats::setNames(
    lapply(vstests, function(vt) {
      vals <- data$CHG[data$VSTEST == vt & !is.na(data$CHG)]
      rng  <- diff(range(vals))
      list(min = floor(min(vals) - pad * rng), max = ceiling(max(vals) + pad * rng))
    }),
    vstests
  )
}

build_vstest_key_map <- function(data) {
  vstests <- sort(unique(data$VSTEST))
  stats::setNames(
    lapply(vstests, function(vt) unique(data$KEY[data$VSTEST == vt])),
    vstests
  )
}
