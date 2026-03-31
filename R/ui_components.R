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
#'     Sets a crosstalk FilterHandle on every group (all pages + All) so only
#'     rows matching the chosen VSTEST are visible. Also calls syncYRange().
#'
#'   switchPage(safeTrt, page)
#'     Shows the selected page-view div and hides the others within a panel.
#'
#'   toggleTreatmentPanels()
#'     Shows or hides treatment panel divs via CSS display. Also calls syncYRange().
#'
#'   syncYRange(vstest)
#'     Applies a shared y-axis range across all visible panels when 2+ are shown.
#'
#' @param vstest_key_map  Named list from build_vstest_key_map().
#' @param all_groups      Character vector of ALL crosstalk group names
#'                        (every trt_All + trt_p1 + trt_p2 … combination).
#' @param trt_groups      Character vector of treatment group names (TRTA levels).
#' @param default_vstest  VSTEST value to activate on page load.
#' @param chg_range_map   Named list from build_chg_range() (VSTEST → {min, max}).
#' @return An htmltools <script> tag.
build_filter_js <- function(vstest_key_map, all_groups, trt_groups, default_vstest, chg_range_map) {
  key_map_json   <- jsonlite::toJSON(vstest_key_map, auto_unbox = FALSE)
  groups_json    <- jsonlite::toJSON(all_groups,     auto_unbox = FALSE)
  trt_json       <- jsonlite::toJSON(trt_groups,     auto_unbox = FALSE)
  range_map_json <- jsonlite::toJSON(chg_range_map,  auto_unbox = TRUE)

  tags$script(HTML(sprintf(
"(function () {
  var VSTEST_KEY_MAP = %s;
  var ALL_GROUPS     = %s;   /* every trt_All + trt_pN group */
  var TRT_GROUPS     = %s;
  var CHG_RANGE_MAP  = %s;

  var vstestHandles = {};

  function initHandles() {
    ALL_GROUPS.forEach(function (g) {
      vstestHandles[g] = new crosstalk.FilterHandle(g);
    });
  }

  function syncYRange(vstest) {
    var panels  = document.querySelectorAll('.treatment-panel');
    var visible = Array.prototype.filter.call(panels, function (p) {
      return p.style.display !== 'none';
    });
    var rng = (visible.length > 1 && CHG_RANGE_MAP[vstest])
      ? [CHG_RANGE_MAP[vstest].min, CHG_RANGE_MAP[vstest].max]
      : null;
    visible.forEach(function (panel) {
      var activeView = panel.querySelector('.page-view[data-page]');
      if (activeView && activeView.style.display === 'none') activeView = null;
      if (!activeView) {
        var views = panel.querySelectorAll('.page-view');
        for (var i = 0; i < views.length; i++) {
          if (views[i].style.display !== 'none') { activeView = views[i]; break; }
        }
      }
      if (!activeView) return;
      var plotDiv = activeView.querySelector('.plotly');
      if (!plotDiv) return;
      if (rng) {
        Plotly.relayout(plotDiv, { 'yaxis.range': rng, 'yaxis.autorange': false });
      } else {
        Plotly.relayout(plotDiv, { 'yaxis.autorange': true });
      }
    });
  }

  window.filterByVStest = function (vstest) {
    var keys = VSTEST_KEY_MAP[vstest];
    ALL_GROUPS.forEach(function (g) {
      if (keys && keys.length > 0) {
        vstestHandles[g].set(keys);
      } else {
        vstestHandles[g].clear();
      }
    });
    syncYRange(vstest);
  };

  /* Show the selected page-view div, hide the rest */
  window.switchPage = function (safeTrt, page) {
    var views = document.querySelectorAll('.page-view-' + safeTrt);
    views.forEach(function (v) {
      v.style.display = (v.getAttribute('data-page') === String(page)) ? 'block' : 'none';
    });
    var vstest = document.getElementById('vstest-select').value;
    syncYRange(vstest);
  };

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

  function attachAfterplotHandlers() {
    document.querySelectorAll('.treatment-panel').forEach(function (panel) {
      panel.querySelectorAll('.page-view').forEach(function (view) {
        var plotDiv = view.querySelector('.plotly');
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

  function init() {
    initHandles();
    /* Hide all non-All page-view divs now that HTMLWidgets has bound everything */
    document.querySelectorAll('.page-view').forEach(function (v) {
      if (v.getAttribute('data-page') !== 'All') v.style.display = 'none';
    });
    filterByVStest('%s');
    attachAfterplotHandlers();
  }

  if (typeof HTMLWidgets !== 'undefined' && HTMLWidgets.addPostRenderHandler) {
    HTMLWidgets.addPostRenderHandler(init);
  } else {
    window.addEventListener('load', init);
  }
}());
",
    key_map_json,
    groups_json,
    trt_json,
    range_map_json,
    default_vstest
  )))
}
