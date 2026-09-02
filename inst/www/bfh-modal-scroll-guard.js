// Vagt mod laast sidescroll efter modal-skift (se R/fct_modal_guard.R):
//  - Bootstrap laaser sidescroll ved at saette overflow:hidden paa <body> naar
//    en modal aabnes, og gendanner ved lukning praecis den vaerdi der stod der
//    da modalen aabnede. Laasen rulles foerst tilbage naar hidden.bs.modal
//    fyrer efter fade-out.
//  - Shiny's showModal() erstatter en allerede aaben modal ved at overskrive
//    modal-wrapperens HTML. Den gamle modals hidden.bs.modal fyrer derfor
//    ALDRIG, og dens laas rulles aldrig tilbage; aabnes en ny modal oven i den,
//    gemmer Bootstrap "hidden" som den vaerdi der skal gendannes bagefter.
//    Begge veje ender med en <body> der bliver staaende med overflow:hidden.
//  - Appen erstatter modaler med vilje flere steder (mod_indikator_crud.R:
//    bekraeftelse af aendret indikator-id, diagram-swap, fortryd der
//    genopbygger formularen), saa vagten rydder op EFTER at sidste modal er
//    lukket i stedet for at forbyde moensteret.
(function() {
  'use strict';

  // Bootstrap's fade-out er ~150 ms. Ventes den ikke af, staar .show endnu paa
  // den modal der er ved at lukke, og vi ville tro at der stadig er en aaben.
  var RYD_DELAY_MS = 250;

  function ryd() {
    // En anden modal naaede at aabne (fx en swap) → dens egen laas skal blive
    // staaende. Der roeres kun ved noget naar ingen modal er tilbage, saa
    // vagten kan ikke rive scroll-laasen vaek under en aaben modal.
    if (document.querySelector('.modal.show')) return;

    document.body.classList.remove('modal-open');
    document.body.style.removeProperty('overflow');
    document.body.style.removeProperty('padding-right');

    // Erstattede modaler kan efterlade deres backdrop i DOM'en; den ville
    // ligge som et usynligt klik-skjold hen over siden.
    var efterladte = document.querySelectorAll('.modal-backdrop');
    for (var i = 0; i < efterladte.length; i++) {
      efterladte[i].parentNode.removeChild(efterladte[i]);
    }
  }

  function planlaegRyd() {
    setTimeout(ryd, RYD_DELAY_MS);
  }

  // Bootstrap 5 fyrer native events der bobler; aeldre (jQuery-baserede)
  // udgaver fyrer kun gennem jQuery. Begge veje lyttes af — ryd() er
  // idempotent, saa et dobbelt kald er harmloest.
  document.addEventListener('hidden.bs.modal', planlaegRyd, true);
  if (window.jQuery) {
    window.jQuery(document).on('hidden.bs.modal', planlaegRyd);
  }
})();
