// Vagt mod laast sidescroll efter en modal (se R/fct_modal_guard.R):
//  - Bootstrap laaser sidescroll ved at saette overflow:hidden paa <body> naar
//    en modal aabnes, og ruller foerst laasen tilbage naar hidden.bs.modal
//    fyrer efter fade-out.
//  - Fyrer det event ikke, bliver laasen staaende, og siden kan ikke scrolles
//    selv om ingen modal er synlig. Shiny's showModal() erstatter fx en
//    allerede aaben modal ved at overskrive wrapperens indhold
//    (renderContentAsync) — den gamle modals hidden.bs.modal fyrer aldrig.
//    Andre veje (afbrudt transition, modal revet ud af DOM'en under et tungt
//    re-render) giver samme slutresultat.
//
// Vagten haenger derfor IKKE alene paa hidden.bs.modal: den ser ogsaa direkte
// paa <body> og reagerer, naar modal-relaterede noder eller klasser aendrer
// sig. Den rydder kun op naar der ikke laengere er en synlig modal, saa den
// kan ikke rive laasen vaek under et bevidst modal-skift (bekraeftelses-
// dialog, diagram-swap, fortryd — se mod_indikator_crud.R).
//
// NB: Dette er et sikkerhedsnet, ikke en forklaring. Staar laasen tilbage,
// er der en modal-lukning et sted der ikke afsluttes ordentligt.
(function() {
  'use strict';

  // Bootstrap's fade-out er ~150 ms. Ventes den ikke af, staar .show endnu paa
  // den modal der er ved at lukke, og vi ville tro at der stadig er en aaben.
  var RYD_DELAY_MS = 250;
  var planlagt = null;

  function harSynligModal() {
    return !!document.querySelector('.modal.show');
  }

  function erLaast() {
    return document.body.classList.contains('modal-open') ||
      document.body.style.overflow === 'hidden' ||
      document.querySelectorAll('.modal-backdrop').length > 0;
  }

  function ryd() {
    planlagt = null;
    // En modal er (stadig) aaben — dens egen laas skal blive staaende.
    if (harSynligModal() || !erLaast()) return;

    document.body.classList.remove('modal-open');
    document.body.style.removeProperty('overflow');
    document.body.style.removeProperty('padding-right');

    // En efterladt backdrop ville ligge som et usynligt klik-skjold over siden.
    var efterladte = document.querySelectorAll('.modal-backdrop');
    for (var i = 0; i < efterladte.length; i++) {
      efterladte[i].parentNode.removeChild(efterladte[i]);
    }
  }

  function planlaegRyd() {
    if (planlagt !== null) return;   // saml mange mutationer til én oprydning
    planlagt = setTimeout(ryd, RYD_DELAY_MS);
  }

  function start() {
    // 1) Den normale vej: Bootstrap 5 fyrer native events der bobler, aeldre
    //    (jQuery-baserede) udgaver kun gennem jQuery. Begge lyttes af —
    //    ryd() er idempotent, saa et dobbelt kald er harmloest.
    document.addEventListener('hidden.bs.modal', planlaegRyd, true);
    if (window.jQuery) {
      window.jQuery(document).on('hidden.bs.modal', planlaegRyd);
    }

    // 2) Sikkerhedsnettet: reagér paa at modal-noder eller body's egne
    //    klasser/style aendrer sig, ogsaa naar intet lukke-event fyrer.
    //    ryd() aendrer selv body, men er en no-op naar der intet er at rydde,
    //    saa den kan ikke holde sig selv koerende.
    if (typeof MutationObserver === 'function') {
      new MutationObserver(planlaegRyd).observe(document.body, {
        childList: true,                        // wrapper/backdrop ind og ud
        attributes: true,
        attributeFilter: ['class', 'style']     // modal-open / inline overflow
      });
    }
  }

  if (document.body) start();
  else document.addEventListener('DOMContentLoaded', start);
})();
