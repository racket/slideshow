
(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide config^ viewer^ core^)

  ;; Main inputs to the core unit:
  (define-signature config^
    (base-font-size
     screen-w screen-h
     actual-screen-w actual-screen-h
     use-screen-w use-screen-h
     
     condense?
     printing?
     commentary?
     show-gauge?
     keep-titlebar?
     show-page-numbers?
     quad-view?
     print-slide-seconds?
     use-transitions? use-offscreen?
     talk-duration-minutes
     trust-me?
     no-squash?
     two-frames?
     use-prefetch?
     use-prefetch-in-preview?
     print-target
     init-page))

  ;; Viewer inputs to the core:
  (define-signature viewer^
    (display-progress
     get-talk-slide-list
     set-talk-slide-list!
     set-init-page!
     set-use-background-frame!
     enable-click-advance!
     set-page-numbers-visible!
     add-click-region!))

  ;; The functions mostly used by a slideshow program:
  (define-signature core^
    (plain-slide
     slide/title
     slide/title/tall
     slide/center
     slide/title/center 
     slide/inset
     slide/title/inset
     slide/title/tall/inset
     slide/center/inset
     slide/title/center/inset
     slide/name 
     slide/name/tall 
     slide/name/center 
     slide/name/inset
     slide/name/tall/inset
     slide/name/center/inset
     
     most-recent-slide retract-most-recent-slide re-slide start-at-recent-slide
     scroll-transition pause-transition
     comment make-outline
     item item* page-item page-item*
     item/bullet item*/bullet page-item/bullet page-item*/bullet
     subitem subitem* page-subitem page-subitem*
     itemize itemize* page-itemize page-itemize*
     para para* page-para page-para*
     para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
     font-size gap-size current-font-size line-sep title-size 
     main-font current-main-font with-font current-title-color
     red green blue purple orange size-in-pixels
     t it bt bit tt titlet tt* rt
     bullet o-bullet
     get-margin set-margin! get-client-w get-client-h
     get-full-page get-titleless-page
     skip-slides
     set-use-background-frame!
     enable-click-advance!
     get-title-h set-title-h! current-slide-assembler
     current-page-number-font current-page-number-color
     set-page-numbers-visible!
     clickback 
     make-slide-inset
     apply-slide-inset)))
