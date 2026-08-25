// Ekko-vaern for excelR-grids (se R/fct_excel_table.R).
//
// excelR's widget-JS sender ved HVERT re-render (alt undtagen foerste render)
// selv et payload paa grid'ets Shiny-input: dels et data-ekko
// (Shiny.setInputValue(container.id, {data: params.data, ...}) sidst i
// renderValue), dels et selektions-ekko naar den gemte markoer genskabes
// (updateSelectionFromCoords -> onselection -> nyt fullData-payload).
//
// Serveren diff'er alle payloads mod sin egen tilstand og re-renderer efter
// gem/afvisning. Ekkoerne baerer aldrig ny information -- de gengiver blot
// det, serveren netop har renderet -- men enhver vedvarende forskel i
// repraesentation (checkbox true/TRUE, tal-, dato- eller entity-formatering)
// bliver til en fantom-aendring, og saa koerer gem -> reload -> re-render ->
// ekko -> gem i ring, uden fejlmeddelelse. Derfor droppes alle payloads som
// et jexcel-output selv affyrer synkront under sit re-render.
(function () {
  "use strict";

  var mutedId = null;

  // shiny:value affyres paa output-elementet umiddelbart FOER bindingens
  // renderValue koerer synkront -- vinduet lukkes i naeste event-loop-tick.
  // Rigtige brugerhandlinger kan ikke forekomme inde i samme synkrone task,
  // saa kun widget'ens egne ekkoer rammer vinduet.
  window.jQuery(document).on("shiny:value.bfhEchoGuard", function (e) {
    var el = e.target;
    if (!el || !el.classList || !el.classList.contains("jexcel")) return;
    mutedId = el.id;
    window.setTimeout(function () { mutedId = null; }, 0);
  });

  window.jQuery(document).one("shiny:sessioninitialized.bfhEchoGuard",
    function () {
      var original = Shiny.setInputValue.bind(Shiny);
      Shiny.setInputValue = function (name, value, opts) {
        if (mutedId !== null && name === mutedId) return; // re-render-ekko
        return original(name, value, opts);
      };
    });
})();
