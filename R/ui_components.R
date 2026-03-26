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
  tags$div(
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
#' The onchange handler calls toggleTreatmentPanels() defined in build_filter_js().
#'
#' @param all_trts   Character vector of unique TRTA values.
#' @param trt_colors Named character vector: treatment name → hex colour.
#' @return An htmltools tag.
build_global_treatment_control <- function(all_trts, trt_colors) {
  tags$div(
    style = "display: inline-flex; align-items: center; gap: 18px; flex-wrap: wrap;",

    tags$span(
      style = paste0(
        "font-weight: bold; font-size: 14px; ",
        "font-family: Georgia, serif; color: #1e293b; white-space: nowrap;"
      ),
      "Treatments"
    ),

    lapply(all_trts, function(trt) {
      col <- trt_colors[[trt]]
      tags$label(
        style = paste0(
          "display: inline-flex; align-items: center; gap: 5px; cursor: pointer; ",
          "font-family: Georgia, serif; font-size: 13px; font-weight: 600; ",
          "color: ", col, "; white-space: nowrap;"
        ),
        tags$input(
          type     = "checkbox",
          class    = "trt-checkbox",
          value    = trt,
          checked  = NA,
          onchange = "toggleTreatmentPanels()"
        ),
        trt
      )
    })
  )
}


#' Inline <script> block for all filter behaviours + y-axis range sync
#'
#' Defines window-level functions used by the HTML controls:
#'
#'   filterByVStest(vstest)
#'     Sets a crosstalk FilterHandle on every treatment group so only rows
#'     matching the chosen VSTEST are visible. Also calls syncYRange().
#'
#'   filterBySubjectPage(trt, page)
#'     Sets or clears a per-treatment crosstalk FilterHandle for subject paging.
#'
#'   toggleTreatmentPanels()
#'     Shows or hides treatment panel divs via CSS display. Also calls syncYRange().
#'
#'   syncYRange(vstest)
#'     Applies a shared y-axis range across all visible panels when 2+ are shown.
#'     Resets to auto-scale when only 1 panel is visible.
#'     Range is stable across subject page changes.
#'
#' @param vstest_key_map  Named list from build_vstest_key_map().
#' @param subj_page_map   Named list from build_subj_page_map().
#' @param trt_groups      Character vector of treatment group names (TRTA levels).
#' @param default_vstest  VSTEST value to activate on page load.
#' @param chg_range_map   Named list from build_chg_range() (VSTEST → {min, max}).
#' @return An htmltools <script> tag.
build_filter_js <- function(vstest_key_map, subj_page_map, trt_groups, default_vstest, chg_range_map) {
  key_map_json   <- jsonlite::toJSON(vstest_key_map, auto_unbox = FALSE)
  page_map_json  <- jsonlite::toJSON(subj_page_map,  auto_unbox = FALSE)
  groups_json    <- jsonlite::toJSON(trt_groups,      auto_unbox = FALSE)
  range_map_json <- jsonlite::toJSON(chg_range_map,   auto_unbox = TRUE)

  tags$script(HTML(sprintf(
"(function () {
  /* ── Data embedded from R ───────────────────────────────────────────── */
  var VSTEST_KEY_MAP = %s;
  var SUBJ_PAGE_MAP  = %s;
  var TRT_GROUPS     = %s;
  var CHG_RANGE_MAP  = %s;   /* VSTEST label → {min, max} across all data  */

  /* ── crosstalk FilterHandles ─────────────────────────────────────────── */
  var vstestHandles   = {};
  var subjPageHandles = {};

  function initHandles() {
    TRT_GROUPS.forEach(function (g) {
      vstestHandles[g]   = new crosstalk.FilterHandle(g);
      subjPageHandles[g] = new crosstalk.FilterHandle(g);
    });
  }

  /* ── Sync y-axis range across all visible panels ─────────────────────── */
  /* Applies shared range when 2+ panels visible; auto-scales for 1 panel. */
  /* Range is fixed across subject page changes (computed from all data).  */
  function syncYRange(vstest) {
    var panels  = document.querySelectorAll('.treatment-panel');
    var visible = Array.prototype.filter.call(panels, function (p) {
      return p.style.display !== 'none';
    });
    var rng = (visible.length > 1 && CHG_RANGE_MAP[vstest])
      ? [CHG_RANGE_MAP[vstest].min, CHG_RANGE_MAP[vstest].max]
      : null;
    visible.forEach(function (panel) {
      var plotDiv = panel.querySelector('.plotly');
      if (!plotDiv) return;
      if (rng) {
        Plotly.relayout(plotDiv, { 'yaxis.range': rng, 'yaxis.autorange': false });
      } else {
        Plotly.relayout(plotDiv, { 'yaxis.autorange': true });
      }
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
    syncYRange(vstest);
  };

  /* ── Per-panel: filter subjects by page (or clear to show all) ───────── */
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

    var vstest = document.getElementById('vstest-select').value;
    syncYRange(vstest);
  };

  /* ── Initialise after ALL htmlwidgets have finished binding ──────────── */
  if (typeof HTMLWidgets !== 'undefined' && HTMLWidgets.addPostRenderHandler) {
    HTMLWidgets.addPostRenderHandler(function () {
      initHandles();
      filterByVStest('%s');
      /* Re-apply shared range after any plotly relayout (highlight, de-highlight) */
      document.querySelectorAll('.treatment-panel').forEach(function (panel) {
        var plotDiv = panel.querySelector('.plotly');
        if (!plotDiv) return;
        var syncing = false;
        plotDiv.on('plotly_afterplot', function () {
          if (syncing) return;
          var panels  = document.querySelectorAll('.treatment-panel');
          var visible = Array.prototype.filter.call(panels, function (p) {
            return p.style.display !== 'none';
          });
          if (visible.length < 2) return;
          var vstest = document.getElementById('vstest-select').value;
          var rng = CHG_RANGE_MAP[vstest];
          if (!rng) return;
          var current = plotDiv._fullLayout && plotDiv._fullLayout.yaxis && plotDiv._fullLayout.yaxis.range;
          if (current && Math.abs(current[0] - rng.min) < 0.001 && Math.abs(current[1] - rng.max) < 0.001) return;
          syncing = true;
          Plotly.relayout(plotDiv, { 'yaxis.range': [rng.min, rng.max], 'yaxis.autorange': false })
            .then(function () { syncing = false; });
        });
      });
    });
  } else {
    window.addEventListener('load', function () {
      initHandles();
      filterByVStest('%s');
      document.querySelectorAll('.treatment-panel').forEach(function (panel) {
        var plotDiv = panel.querySelector('.plotly');
        if (!plotDiv) return;
        var syncing = false;
        plotDiv.on('plotly_afterplot', function () {
          if (syncing) return;
          var panels  = document.querySelectorAll('.treatment-panel');
          var visible = Array.prototype.filter.call(panels, function (p) {
            return p.style.display !== 'none';
          });
          if (visible.length < 2) return;
          var vstest = document.getElementById('vstest-select').value;
          var rng = CHG_RANGE_MAP[vstest];
          if (!rng) return;
          var current = plotDiv._fullLayout && plotDiv._fullLayout.yaxis && plotDiv._fullLayout.yaxis.range;
          if (current && Math.abs(current[0] - rng.min) < 0.001 && Math.abs(current[1] - rng.max) < 0.001) return;
          syncing = true;
          Plotly.relayout(plotDiv, { 'yaxis.range': [rng.min, rng.max], 'yaxis.autorange': false })
            .then(function () { syncing = false; });
        });
      });
    });
  }
}());
",
    key_map_json,
    page_map_json,
    groups_json,
    range_map_json,
    default_vstest,
    default_vstest
  )))
}
