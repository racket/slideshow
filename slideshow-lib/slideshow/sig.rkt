
(module sig mzscheme
  (require mzlib/unit)

  (provide config^ viewer^ core^
	   cmdline^)

  ;; Configuration inputs to the core unit:
  (define-signature config^
    (base-font-size              ; normally 32
     screen-ws screen-hs         ; aspect -> logical size; 'fullscreen -> 1024x768, 'widescreen -> 1360x766, #f varies
     use-screen-ws use-screen-hs ; aspect -> "pixel" size
     pixel-scale                 ; amount the "pixels" are scaled (e.g., for quad)
     condense? printing?         ; mode
     smoothing?
     commentary-on-slide?))

  ;; Viewer inputs to the core unit:
  (define-signature viewer^
    (;; Registering slides:
     add-talk-slide!
     retract-talk-slide!
     most-recent-talk-slide
     ;; Pass-through of slide-program requests:
     set-init-page!
     set-use-background-frame!
     enable-click-advance!
     set-page-numbers-visible!
     done-making-slides
     set-spotlight-style!
     set-allow-new-slides-after-close!
     ;; Called when a clickback-containing slide is rendered:
     add-click-region!
     ;; Called when a interactive-containing slide is rendered:
     add-interactive!
     ;; To potentially speed up display:
     pict->pre-render-pict))

  ;; The core unit's exports, which are the functions used by a
  ;; Slideshow program:
  (define-signature core^
    (slide/kw
     slide
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
     slide/timeout
     slide/title/timeout
     slide/center/timeout
     slide/title/center/timeout

     most-recent-slide retract-most-recent-slide re-slide slide->pict start-at-recent-slide
     scroll-transition pause-transition
     comment make-outline
     item/kw item item* page-item page-item*
     item/bullet item*/bullet page-item/bullet page-item*/bullet
     subitem/kw subitem subitem* page-subitem page-subitem*
     itemize itemize* page-itemize page-itemize*
     para/kw para para* page-para page-para*
     para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
     font-size gap-size current-gap-size current-font-size current-line-sep line-sep title-size
     commentary-on-slide-font-size
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
     current-page-number-font current-page-number-color current-page-number-adjust
     current-titlet current-para-widths get-current-para-width
     set-page-numbers-visible! done-making-slides
     set-spotlight-style!
     set-allow-new-slides-after-close!
     pict->pre-render-pict
     clickback
     interactive
     make-slide-inset
     apply-slide-inset
     condense?
     printing?))

  ;; ----------------------------------------

  ;; Extra cmdline inputs to a viewer:
  (define-signature cmdline^ extends config^
    (file-to-load ; #f or a path/string
     init-page
     use-transitions?
     print-slide-seconds?
     show-page-numbers?
     show-time?
     show-elapsed-time?
     commentary?
     use-offscreen?
     actual-screen-w actual-screen-h ; actual size (center use- within here)
     auto-screen-size? ; auto-resize
     trust-me?
     quad-view?
     keep-titlebar?
     right-half-screen?
     two-frames?
     use-prefetch?
     use-prefetch-in-preview?
     print-target
     progress-mode
     talk-duration-minutes
     screen-number
     letterbox-color)))
