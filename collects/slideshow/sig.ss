
(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide config^ cmdline^ viewer^ core^)

  ;; Main inputs to the core unit:
  (define-signature config^
    (file-to-load

     base-font-size
     screen-w screen-h          ; logical size
     use-screen-w use-screen-h  ; pixel size
     
     condense?
     printing?
     use-transitions? 
     init-page))

  (define-signature cmdline^
    ((open config^)
     print-slide-seconds?
     show-page-numbers?
     commentary?
     use-offscreen?
     actual-screen-w actual-screen-h ; actual size (center use- within here)
     trust-me?
     quad-view?
     keep-titlebar?
     two-frames?
     use-prefetch?
     use-prefetch-in-preview?
     print-target
     talk-duration-minutes))

  ;; Viewer inputs to the core:
  (define-signature viewer^
    (;; Registering slides:
     set-talk-slide-list!
     get-talk-slide-list
     display-progress
     ;; Pass-through of user-program requests:
     set-init-page!
     set-use-background-frame!
     enable-click-advance!
     set-page-numbers-visible!
     ;; Called when a clickback-containing slide is rendered:
     add-click-region!

     ;; Not for the core; exported by "slideshow.ss", instead:
     start-making-slides
     done-making-slides
     started-from-launcher))

  ;; The functions used by a slideshow program:
  (define-signature core^
    (slide
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
     apply-slide-inset
     condense?
     printing?)))
