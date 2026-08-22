(function () {
  "use strict";

  const states = new WeakMap();

  function wrapperFor(container) {
    return container.closest(".bfh-excel-grid") || container;
  }

  function isOptInContainer(container) {
    const owner = container && container.closest(".bfh-excel-grid");
    return !!(owner && owner.dataset.bfhAdapter === "true");
  }

  function isConnected() {
    return !!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$socket &&
      Shiny.shinyapp.$socket.readyState === 1);
  }

  function lockGrid(container, state, message, cell) {
    const wrapper = wrapperFor(container);
    wrapper.classList.add("bfh-grid-locked");
    wrapper.title = message;
    state.grid.options.editable = false;
    if (cell) cell.title = message;
  }

  function disconnectState(container, state, cell) {
    state.savedTimers.forEach(function (timer) {
      window.clearTimeout(timer);
    });
    state.savedTimers.clear();
    state.latestByCell.clear();
    state.eventDetails.clear();
    state.grid.records.forEach(function (row) {
      row.forEach(function (record) {
        const transient = record.classList.contains("bfh-cell-pending") ||
          record.classList.contains("bfh-cell-saved");
        record.classList.remove("bfh-cell-pending", "bfh-cell-saved");
        if (transient) record.removeAttribute("title");
      });
    });
    lockGrid(container, state,
      "Forbindelsen til serveren er afbrudt. Genindl\u00E6s siden.", cell);
  }

  function clearSavedTimer(state, cellKey) {
    const timer = state.savedTimers.get(cellKey);
    if (timer) window.clearTimeout(timer);
    state.savedTimers.delete(cellKey);
  }

  function markPending(state, cellKey, cell) {
    clearSavedTimer(state, cellKey);
    cell.classList.remove("bfh-cell-saved", "bfh-cell-rejected");
    cell.classList.add("bfh-cell-pending");
    cell.title = "Gemmer \u00E6ndringen\u2026";
  }

  function currentRowForPk(grid, rowPk) {
    const expected = String(rowPk);
    for (let y = 0; y < grid.records.length; y += 1) {
      if (String(grid.getValueFromCoords(0, y)) === expected) return y;
    }
    return -1;
  }

  function requestedGeneration(container) {
    const value = Number(container.dataset.bfhGeneration);
    return Number.isInteger(value) ? value : null;
  }

  function attach(container) {
    if (!isOptInContainer(container)) return;
    const grid = container.excel;
    const generation = requestedGeneration(container);
    if (!grid || generation === null) return;
    const old = states.get(container);
    if (old && old.grid === grid && old.generation === generation) return;
    const state = {
      grid: grid,
      generation: generation,
      sequence: 0,
      latestByCell: new Map(),
      eventDetails: new Map(),
      suppressChange: false,
      savedTimers: new Map()
    };
    states.set(container, state);

    grid.options.onchange = function (_instance, _cell, x, y, value) {
      if (state.suppressChange) return;
      const columnIndex = Number(x);
      const rowIndex = Number(y);
      const rowPk = grid.getValueFromCoords(0, rowIndex);
      const cellKey = String(rowPk) + ":" + columnIndex;
      const eventId = generation + ":" + (++state.sequence);
      const cell = grid.records[rowIndex] && grid.records[rowIndex][columnIndex];
      state.latestByCell.set(cellKey, eventId);
      state.eventDetails.set(eventId,
        { cellKey: cellKey, rowPk: rowPk, columnIndex: columnIndex });
      if (cell) markPending(state, cellKey, cell);

      if (!isConnected()) {
        disconnectState(container, state, cell);
        return;
      }
      Shiny.setInputValue(container.id + "_cell", {
        event_id: eventId,
        grid_generation: generation,
        row_pk: rowPk,
        column_index: columnIndex,
        raw_value: value
      }, { priority: "event" });
    };

    grid.options.onselection = function (_instance, x1, y1, x2, y2) {
      const top = Math.min(Number(y1), Number(y2));
      const bottom = Math.max(Number(y1), Number(y2));
      const left = Math.min(Number(x1), Number(x2));
      const right = Math.max(Number(x1), Number(x2));
      const rowPks = [];
      const seen = new Set();
      for (let y = top; y <= bottom; y += 1) {
        const rowPk = grid.getValueFromCoords(0, y);
        const key = String(rowPk);
        if (!seen.has(key)) {
          seen.add(key);
          rowPks.push(rowPk);
        }
      }
      if (!isConnected()) {
        disconnectState(container, state);
        return;
      }
      Shiny.setInputValue(container.id + "_selection", {
        grid_generation: generation,
        boundaries: { top: top, bottom: bottom, left: left, right: right },
        row_pks: rowPks
      }, { priority: "event" });
    };
  }

  function scan() {
    document.querySelectorAll(
      ".bfh-excel-grid[data-bfh-adapter=\"true\"] .jexcel_container"
    ).forEach(attach);
  }

  Shiny.addCustomMessageHandler("bfh-excel-adapter:init", function (payload) {
    const container = document.getElementById(payload.id);
    if (!isOptInContainer(container)) return;
    container.dataset.bfhGeneration = String(payload.grid_generation);
    attach(container);
  });

  Shiny.addCustomMessageHandler("bfh-excel-adapter:result", function (message) {
    const container = document.getElementById(message.id);
    if (!container) return;
    const state = states.get(container);
    if (!state || state.generation !== Number(message.grid_generation)) return;
    if (message.lock_grid === true) {
      lockGrid(container, state,
        "Gridet er l\u00E5st. Genindl\u00E6s siden for at forts\u00E6tte.");
    }
    const details = state.eventDetails.get(message.event_id);
    if (!details) return;
    state.eventDetails.delete(message.event_id);
    if (state.latestByCell.get(details.cellKey) !== message.event_id) return;
    if (message.status !== "saved" && message.status !== "rejected") {
      lockGrid(container, state,
        "Serveren sendte et ugyldigt svar. Genindl\u00E6s siden.");
      return;
    }
    const y = currentRowForPk(state.grid, details.rowPk);
    const x = Number(details.columnIndex);
    if (y < 0 || !state.grid.records[y] || !state.grid.records[y][x]) {
      lockGrid(container, state,
        "R\u00E6kken kan ikke findes. Genindl\u00E6s siden.");
      return;
    }
    const cell = state.grid.records[y][x];
    state.suppressChange = true;
    try {
      state.grid.setValueFromCoords(x, y,
        message.value == null ? "" : message.value);
    } finally {
      state.suppressChange = false;
    }
    cell.classList.remove("bfh-cell-pending", "bfh-cell-saved", "bfh-cell-rejected");
    clearSavedTimer(state, details.cellKey);
    if (message.status === "saved") {
      cell.classList.add("bfh-cell-saved");
      cell.title = "\u00C6ndringen er gemt.";
      const eventId = message.event_id;
      state.savedTimers.set(details.cellKey, window.setTimeout(function () {
        if (state.latestByCell.get(details.cellKey) === eventId) {
          cell.classList.remove("bfh-cell-saved");
          cell.removeAttribute("title");
        }
        state.savedTimers.delete(details.cellKey);
      }, 900));
    } else if (message.status === "rejected") {
      cell.classList.add("bfh-cell-rejected");
      cell.title = typeof message.message === "string" && message.message.length > 0 ?
        message.message : "\u00C6ndringen blev afvist.";
    }
  });

  new MutationObserver(scan).observe(document.documentElement,
    { childList: true, subtree: true });
  window.jQuery(document).on("shiny:disconnected.bfhExcelAdapter", function () {
    document.querySelectorAll(
      ".bfh-excel-grid[data-bfh-adapter=\"true\"] .jexcel_container"
    ).forEach(function (container) {
      const state = states.get(container);
      if (state) disconnectState(container, state);
    });
  });
  document.addEventListener("shiny:connected", scan);
  document.addEventListener("DOMContentLoaded", scan);
})();
