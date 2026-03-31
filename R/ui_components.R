build_global_parameter_control <- function(all_vstests) {
  tags$div(
    style = "display:inline-flex; align-items:center; gap:10px;",
    tags$label(`for` = "vstest-select",
               style = "font-weight:bold; font-size:14px; font-family:Georgia,serif; color:#1e293b; white-space:nowrap;",
               "Parameter (VSTEST)"),
    tags$select(
      id = "vstest-select", onchange = "filterByVStest(this.value)",
      style = "padding:5px 10px; border:1px solid #cbd5e1; border-radius:5px; font-family:Georgia,serif; font-size:13px; color:#1e293b; background:#fff; cursor:pointer; min-width:180px;",
      lapply(all_vstests, function(vt) tags$option(value = vt, vt))
    )
  )
}

build_global_treatment_control <- function(all_trts, trt_colors) {
  tags$div(
    style = "display:inline-flex; align-items:center; gap:18px; flex-wrap:wrap;",
    tags$span(style = "font-weight:bold; font-size:14px; font-family:Georgia,serif; color:#1e293b; white-space:nowrap;",
              "Treatments"),
    lapply(all_trts, function(trt) {
      tags$label(
        style = paste0("display:inline-flex; align-items:center; gap:5px; cursor:pointer; font-family:Georgia,serif; font-size:13px; font-weight:600; color:", trt_colors[[trt]], "; white-space:nowrap;"),
        tags$input(type = "checkbox", class = "trt-checkbox", value = trt,
                   checked = NA, onchange = "toggleTreatmentPanels()"),
        trt
      )
    })
  )
}

#' @param vstest_key_map  Named list: VSTEST → KEY vector.
#' @param all_groups      All crosstalk group names (trt_All + trt_pN).
#' @param default_vstest  VSTEST selected on load.
#' @param chg_range_map   Named list: VSTEST → list(min, max).
build_filter_js <- function(vstest_key_map, all_groups, trt_groups, default_vstest, chg_range_map) {
  tags$script(HTML(sprintf(
"(function () {
  var VSTEST_KEY_MAP = %s;
  var ALL_GROUPS     = %s;
  var CHG_RANGE_MAP  = %s;

  var handles = {};
  ALL_GROUPS.forEach(function (g) { handles[g] = new crosstalk.FilterHandle(g); });

  function getVstest() { return document.getElementById('vstest-select').value; }

  function syncYRange(vstest) {
    var panels  = document.querySelectorAll('.treatment-panel');
    var visible = Array.prototype.filter.call(panels, function (p) { return p.style.display !== 'none'; });
    var rng = (visible.length > 1 && CHG_RANGE_MAP[vstest])
      ? [CHG_RANGE_MAP[vstest].min, CHG_RANGE_MAP[vstest].max] : null;
    visible.forEach(function (panel) {
      var views = panel.querySelectorAll('.page-view');
      var active;
      for (var i = 0; i < views.length; i++) {
        if (views[i].style.display !== 'none') { active = views[i]; break; }
      }
      if (!active) return;
      var plotDiv = active.querySelector('.plotly');
      if (!plotDiv) return;
      rng ? Plotly.relayout(plotDiv, { 'yaxis.range': rng, 'yaxis.autorange': false })
          : Plotly.relayout(plotDiv, { 'yaxis.autorange': true });
    });
  }

  window.filterByVStest = function (vstest) {
    var keys = VSTEST_KEY_MAP[vstest];
    ALL_GROUPS.forEach(function (g) {
      keys && keys.length ? handles[g].set(keys) : handles[g].clear();
    });
    syncYRange(vstest);
  };

  window.switchPage = function (safeTrt, page) {
    document.querySelectorAll('.page-view-' + safeTrt).forEach(function (v) {
      v.style.display = (v.getAttribute('data-page') === String(page)) ? 'block' : 'none';
    });
    syncYRange(getVstest());
  };

  window.toggleTreatmentPanels = function () {
    var checked = Array.prototype.slice
      .call(document.querySelectorAll('.trt-checkbox:checked'))
      .map(function (cb) { return cb.value; });
    document.querySelectorAll('.treatment-panel').forEach(function (panel) {
      panel.style.display =
        (!checked.length || checked.indexOf(panel.getAttribute('data-treatment')) !== -1)
          ? 'flex' : 'none';
    });
    syncYRange(getVstest());
  };

  function init() {
    document.querySelectorAll('.page-view').forEach(function (v) {
      if (v.getAttribute('data-page') !== 'All') v.style.display = 'none';
    });
    filterByVStest('%s');
    document.querySelectorAll('.page-view .plotly').forEach(function (plotDiv) {
      var syncing = false;
      plotDiv.on('plotly_afterplot', function () {
        if (syncing) return;
        var panels  = document.querySelectorAll('.treatment-panel');
        var visible = Array.prototype.filter.call(panels, function (p) { return p.style.display !== 'none'; });
        if (visible.length < 2) return;
        var rng = CHG_RANGE_MAP[getVstest()];
        if (!rng) return;
        var cur = plotDiv._fullLayout && plotDiv._fullLayout.yaxis && plotDiv._fullLayout.yaxis.range;
        if (cur && Math.abs(cur[0] - rng.min) < 0.001 && Math.abs(cur[1] - rng.max) < 0.001) return;
        syncing = true;
        Plotly.relayout(plotDiv, { 'yaxis.range': [rng.min, rng.max], 'yaxis.autorange': false })
          .then(function () { syncing = false; });
      });
    });
  }

  if (typeof HTMLWidgets !== 'undefined' && HTMLWidgets.addPostRenderHandler) {
    HTMLWidgets.addPostRenderHandler(init);
  } else {
    window.addEventListener('load', init);
  }
}());
",
    jsonlite::toJSON(vstest_key_map, auto_unbox = FALSE),
    jsonlite::toJSON(all_groups,     auto_unbox = FALSE),
    jsonlite::toJSON(chg_range_map,  auto_unbox = TRUE),
    default_vstest
  )))
}
