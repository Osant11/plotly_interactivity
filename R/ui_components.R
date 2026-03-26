# ── Global UI control builders (pure htmltools — no Shiny) ────────────────────
#
# Two global controls sit above all treatment panels:
#   1. Parameter selector  — HTML <select> wired to the JS filterByVStest().
#   2. Treatment selector  — HTML checkboxes wired to toggleTreatmentPanels().
#
# A third JavaScript block enables both controls by bridging the plain HTML
# elements to crosstalk's FilterHandle API and CSS display toggling.


#' Styled HTML <select> for the parameter (VSTEST) — global control
#'
#' The onchange handler calls filterByVStest() defined in build_filter_js().
#' No "All" option: only distinct VSTEST values from the dataset.
#'
#' @param all_vstests  Sorted character vector of unique VSTEST values.
#' @return An htmltools tag.
build_global_parameter_control <- function(all_vstests) {
  tags$div( # nolint
    style = "display: inline-flex; align-items: center; gap: 10px;",

    tags$label(
      `for` = "vstest-select",
      style = paste0(
        "font-weight: bold; font-size: 14px; ",
        "font-family: Georgia, serif; color: #1e293b; white-space: nowrap;"
      ),
      "Parameter (VSTEST)"
    ),

    tags$select(
      id       = "vstest-select",
      onchange = "filterByVStest(this.value)",
      style    = paste0(
        "padding: 5px 10px; border: 1px solid #cbd5e1; border-radius: 5px; ",
        "font-family: Georgia, serif; font-size: 13px; color: #1e293b; ",
        "background: #fff; cursor: pointer; min-width: 180px;"
      ),
      lapply(all_vstests, function(vt) tags$option(value = vt, vt))
    )
  )
}


#' Styled HTML checkboxes for treatment visibility — global control
#'
#' All treatments are pre-ticked. Unchecking one hides that panel.
#' The onchange handler calls toggleTreatmentPanels() defined in build_filter_js().  # nolint
#'
#' @param all_trts   Character vector of unique TRTA values.
#' @param trt_colors Named character vector: treatment name → hex colour.
#' @return An htmltools tag.
build_global_treatment_control <- function(all_trts, trt_colors) {
  tags$div( # nolint
    style = "display: inline-flex; align-items: center; gap: 18px; flex-wrap: wrap;", # nolint

    tags$span(
      style = paste0(
        "font-weight: bold; font-size: 14px; ",
        "font-family: Georgia, serif; color: #1e293b; white-space: nowrap;"
      ),
      "Treatments"
    ),

    lapply(all_trts, function(trt) {
      col <- trt_colors[[trt]]
      tags$label( # nolint
        style = paste0(
          "display: inline-flex; align-items: center; gap: 5px; cursor: pointer; ", # nolint
          "font-family: Georgia, serif; font-size: 13px; font-weight: 600; ",
          "color: ", col, "; white-space: nowrap;"
        ),
        tags$input(
          type     = "checkbox",
          class    = "trt-checkbox",
          value    = trt,
          checked  = NA,               # pre-ticked on render
          onchange = "toggleTreatmentPanels()"
        ),
        trt
      )
    })
  )
}


#' Inline <script> block for all three filter behaviours
#'
#' Defines three window-level functions used by the HTML controls:
#'
#'   filterByVStest(vstest)
#'     Sets a crosstalk FilterHandle on every treatment group so only rows
#'     matching the chosen VSTEST are visible.
#'
#'   filterBySubjectPage(trt, page)
#'     Sets or clears a per-treatment crosstalk FilterHandle for subject paging.
#'     page = "" → FilterHandle.clear() (all subjects restored).
#'     This replaces crosstalk's filter_select because selectize single-select
#'     cannot be cleared back to "nothing selected" once a value is chosen.
#'
#'   toggleTreatmentPanels()
#'     Shows or hides treatment panel divs via CSS display.
#'
#' Initialisation uses HTMLWidgets.addPostRenderHandler(), which fires only
#' after every htmlwidget on the page has completed its JavaScript binding.
#' This is the correct hook for applying the initial VSTEST filter — earlier
#' hooks (DOMContentLoaded, setTimeout 0) fire before crosstalk group
#' subscriptions are registered, leaving the initial filter silently ignored.
#'
#' @param vstest_key_map  Named list from build_vstest_key_map().
#' @param subj_page_map   Named list from build_subj_page_map()
#'                        (treatment → page label → KEY vector).
#' @param trt_groups      Character vector of treatment group names (TRTA levels). # nolint
#' @param default_vstest  VSTEST value to activate on page load.
#' @return An htmltools <script> tag.
build_filter_js <- function(vstest_key_map, subj_page_map, trt_groups, default_vstest) { # nolint
  key_map_json  <- jsonlite::toJSON(vstest_key_map, auto_unbox = FALSE)
  page_map_json <- jsonlite::toJSON(subj_page_map,  auto_unbox = FALSE)
  groups_json   <- jsonlite::toJSON(trt_groups,      auto_unbox = FALSE)

  tags$script(HTML(sprintf( # nolint
"(function () {  # nolint
  /* ── Data embedded from R ───────────────────────────────────────────── */
  var VSTEST_KEY_MAP = %s;   /* VSTEST label → array of compound KEYs      */
  var SUBJ_PAGE_MAP  = %s;   /* treatment → page label → array of KEYs     */
  var TRT_GROUPS     = %s;   /* treatment group names matching SharedData   */

  /* ── crosstalk FilterHandles ─────────────────────────────────────────── */
  /* Two independent handles per group so VSTEST and subject-page filters  */
  /* can be set/cleared independently; crosstalk ANDs them automatically.  */
  var vstestHandles   = {};
  var subjPageHandles = {};

  function initHandles() {
    TRT_GROUPS.forEach(function (g) {
      vstestHandles[g]   = new crosstalk.FilterHandle(g);
      subjPageHandles[g] = new crosstalk.FilterHandle(g);
    });
  }

  /* ── Global: filter every panel by the chosen VSTEST ────────────────── */
  window.filterByVStest = function (vstest) {
    var keys = VSTEST_KEY_MAP[vstest];
    TRT_GROUPS.forEach(function (g) {
      if (keys && keys.length > 0) {
        vstestHandles[g].set(keys);
      } else {
        vstestHandles[g].clear();
      }
    });
  };

  /* ── Per-panel: filter subjects by page (or clear to show all) ───────── */
  /* Called by the plain HTML <select> inside each treatment panel.        */
  /* page = \"\" means \"All subjects\" → explicit clear so the filter is    */
  /* fully removed (selectize single-select cannot reach this state).      */
  window.filterBySubjectPage = function (trt, page) {
    var handle = subjPageHandles[trt];
    if (!handle) return;
    if (!page) {
      handle.clear();
      return;
    }
    var trtMap = SUBJ_PAGE_MAP[trt];
    var keys   = trtMap ? trtMap[page] : null;
    if (keys && keys.length > 0) {
      handle.set(keys);
    } else {
      handle.clear();
    }
  };

  /* ── Global: show / hide treatment panel divs ────────────────────────── */
  window.toggleTreatmentPanels = function () {
    var checked = Array.prototype.slice
      .call(document.querySelectorAll('.trt-checkbox:checked'))
      .map(function (cb) { return cb.value; });

    document.querySelectorAll('.treatment-panel').forEach(function (panel) {
      var trt = panel.getAttribute('data-treatment');
      panel.style.display =
        (checked.length === 0 || checked.indexOf(trt) !== -1)
          ? 'flex' : 'none';
    });
  };

  /* ── Initialise after ALL htmlwidgets have finished binding ──────────── */
  /* HTMLWidgets.addPostRenderHandler fires only once every widget on the  */
  /* page has completed its JS binding and registered its crosstalk group  */
  /* subscriptions.  Earlier hooks (DOMContentLoaded, setTimeout 0) fire  */
  /* before those subscriptions exist, so the initial filter is ignored.  */
  if (typeof HTMLWidgets !== 'undefined' && HTMLWidgets.addPostRenderHandler) {
    HTMLWidgets.addPostRenderHandler(function () {
      initHandles();
      filterByVStest('%s');
    });
  } else {
    /* Fallback for non-widget contexts (e.g. plain browser without htmlwidgets) */ # nolint
    window.addEventListener('load', function () {
      initHandles();
      filterByVStest('%s');
    });
  }
}());
",
    key_map_json,
    page_map_json,
    groups_json,
    default_vstest,
    default_vstest   # repeated for the fallback branch
  )))
}
