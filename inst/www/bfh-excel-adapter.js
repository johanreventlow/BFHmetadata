(function () {
  "use strict";

  const states = new WeakMap();

  function requestedGeneration(container) {
    const value = Number(container.dataset.bfhGeneration);
    return Number.isInteger(value) ? value : null;
  }

  function attach(container) {
    const grid = container.excel;
    const generation = requestedGeneration(container);
    if (!grid || generation === null) return;
    const old = states.get(container);
    if (old && old.grid === grid && old.generation === generation) return;
    states.set(container, { grid: grid, generation: generation });
    // Celle- og selektionscallbacks komponeres i Task 3.
  }

  function scan() {
    document.querySelectorAll(".bfh-excel-grid .jexcel_container").forEach(attach);
  }

  Shiny.addCustomMessageHandler("bfh-excel-adapter:init", function (payload) {
    const container = document.getElementById(payload.id);
    if (!container) return;
    container.dataset.bfhGeneration = String(payload.grid_generation);
    attach(container);
  });

  new MutationObserver(scan).observe(document.documentElement,
    { childList: true, subtree: true });
  document.addEventListener("shiny:connected", scan);
  document.addEventListener("DOMContentLoaded", scan);
})();
