// Flydende reconnect-oplevelse (se R/fct_reconnect.R):
//  - Intet moerkt overlay naar forbindelsen til Shiny-processen tabes;
//    i stedet en diskret toast i hjoernet.
//  - Aktiv fane gemmes loebende i sessionStorage og genaabnes efter
//    genindlaesning (serveren lytter paa input$bfh_restore_nav).
//  - Klienten poller serveren og genindlaeser siden usynligt naar den
//    svarer igen. Staar en modal aaben, ventes paa et klik, saa
//    halvfaerdig indtastning ikke forsvinder bag om ryggen paa brugeren.
(function() {
  'use strict';

  var NAV_KEY = 'bfhmeta.nav';
  var POLL_START_MS = 1000;
  var POLL_MAX_MS = 8000;

  function huskFane(value) {
    try { sessionStorage.setItem(NAV_KEY, value); } catch (e) {}
  }
  function gemtFane() {
    try { return sessionStorage.getItem(NAV_KEY); } catch (e) { return null; }
  }

  // page_navbar'ens tabset har root-id "nav" -- foelg med i faneskift her.
  $(document).on('shiny:inputchanged', function(e) {
    if (e.name === 'nav' && typeof e.value === 'string') huskFane(e.value);
  });

  $(document).on('shiny:sessioninitialized', function() {
    var fane = gemtFane();
    if (fane) Shiny.setInputValue('bfh_restore_nav', fane, {priority: 'event'});
  });

  // --- diskret toast (ren DOM: Shiny er doed naar den skal vises) ---------
  var toastEl = null;
  function visToast(html) {
    if (!toastEl) {
      toastEl = document.createElement('div');
      toastEl.id = 'bfh-reconnect-toast';
      document.body.appendChild(toastEl);
    }
    toastEl.innerHTML = html;
  }

  function modalErAaben() {
    return !!document.querySelector('.modal.show');
  }

  var genopretter = false;
  $(document).on('shiny:disconnected', function(e) {
    if (genopretter) return;
    genopretter = true;
    e.preventDefault(); // undertryk Shinys graa overlay
    visToast('<span class="bfh-reconnect-spinner"></span>' +
             'Forbindelsen blev afbrudt \u2014 genopretter\u2026');

    var ventetid = POLL_START_MS;

    function proev() {
      // I en skjult fane udskydes genindlaesning til fanen ses igen --
      // ellers reloader baggrundsfaner i det uendelige efter dvale.
      if (document.visibilityState === 'hidden') {
        document.addEventListener('visibilitychange', vedSynlig);
        return;
      }
      fetch(window.location.href, { cache: 'no-store' })
        .then(function(r) { if (r.ok) { klar(); } else { igen(); } })
        .catch(igen);
    }
    function igen() {
      ventetid = Math.min(ventetid * 1.5, POLL_MAX_MS);
      setTimeout(proev, ventetid);
    }
    function vedSynlig() {
      if (document.visibilityState === 'visible') {
        document.removeEventListener('visibilitychange', vedSynlig);
        proev();
      }
    }
    function klar() {
      if (modalErAaben()) {
        // Halvfaerdig indtastning i en modal maa ikke forsvinde ved en
        // stille genindlaesning -- giv brugeren chancen for at kopiere
        // teksten og selv vaelge hvornaar.
        visToast('Forbindelsen er klar igen. ' +
          '<button type="button" id="bfh-reconnect-nu" ' +
          'class="btn btn-sm btn-light">Genindl\u00e6s</button>');
        document.getElementById('bfh-reconnect-nu')
          .addEventListener('click', function() { location.reload(); });
      } else {
        location.reload();
      }
    }

    setTimeout(proev, POLL_START_MS);
  });
})();
