(module base scheme/base
  (require "slide.rkt"
           scheme/gui/base
           "private/aspect.rkt"
           (only-in "core.rkt"
                    sliderec?
                    just-a-comment?))

  (current-line-sep 5)

  (case (system-type)
    [(macosx)
     (current-main-font '("Gill Sans" . swiss))]
    [(windows)
     (current-main-font '("Tahoma" . swiss))])

  (define current-para-width (hash-ref current-para-widths #f))
  (for ([(aspect current-para-width) (in-hash current-para-widths)])
    (current-para-width (* 3/4 (get-client-w #:aspect aspect))))

  (current-titlet (lambda (s)
                    (colorize (text s (current-main-font) 40)
                              (current-title-color))))
  (set-title-h! (pict-height (titlet "Hi")))
  (current-title-color "black")

  (provide (rename-out [slide/kw slide]
                       [sliderec? slide?]
                       [just-a-comment? comment?])
           comment
           most-recent-slide retract-most-recent-slide re-slide slide->pict start-at-recent-slide
	   make-outline
	   (rename-out [item/kw item]
                       [subitem/kw subitem]
                       [para/kw para])
	   gap-size current-gap-size current-font-size current-line-sep
	   current-main-font current-title-color
	   size-in-pixels
	   t it bt bit tt titlet tt* rt
	   bullet o-bullet
	   margin set-margin! 
	   client-w client-h
	   full-page titleless-page
	   printing? condense?
	   skip-slides
	   set-use-background-frame!
	   enable-click-advance!
           commentary-on-slide-font-size
	   title-h set-title-h! current-slide-assembler
	   current-page-number-font current-page-number-color current-page-number-adjust
           current-titlet current-para-width
	   set-page-numbers-visible! done-making-slides
           set-spotlight-style! set-allow-new-slides-after-close!
           pict->pre-render-pict
           clickback interactive make-slide-inset slide-inset? apply-slide-inset
           get-client-w get-client-h get-full-page get-titleless-page get-current-para-width
           aspect?))
