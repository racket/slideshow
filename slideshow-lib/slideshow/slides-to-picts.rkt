(module slides-to-picts scheme/base
  (require racket/draw
           scheme/class
           scheme/unit
           "sig.rkt"
           "param.rkt"
           "core.rkt"
           texpict/mrpict)

  (provide get-slides-as-picts)

  (define-namespace-anchor anchor)

  (define get-slides-as-picts
    (lambda (file w h c? [stop-after #f])
      (let ([ns (make-base-namespace)]
	    [orig-ns (namespace-anchor->empty-namespace anchor)]
	    [slides null]
	    [xs (/ w 1024)]
	    [ys (/ h 768)]
	    [escape void])
	(parameterize ([current-namespace ns])
	  (namespace-attach-module orig-ns 'slideshow/param)
	  (namespace-attach-module orig-ns 'slideshow/core))
	(current-slideshow-linker
	 (lambda (core@)
	   (compound-unit
	    (import)
	    (export CORE CONFIG VIEWER)
	    (link [((CONFIG : config^)) (unit 
                                          (import)
                                          (export config^)
                                          (define base-font-size 32)
                                          (define screen-ws #hasheq((#f . 1024) (fullscreen . 1024) (widescreen . 1360)))
                                          (define screen-hs #hasheq((#f .  768) (fullscreen .  768) (widescreen .  766)))
                                          (define (all-aspects v)
                                            (for/hasheq ([aspect (in-hash-keys screen-ws)])
                                              (values aspect v)))
                                          (define use-screen-ws (all-aspects w))
                                          (define use-screen-hs (all-aspects h))
                                          (define use-wide-screen-ws (all-aspects w))
                                          (define use-wide-screen-hs (all-aspects h))
                                          (define pixel-scale 1)
                                          (define condense? c?)
                                          (define printing? #f)
                                          (define smoothing? #t)
                                          (define commentary-on-slide? #f))]
		  [((CORE : core^)) core@ CONFIG VIEWER]
		  [((VIEWER : viewer^)) (unit
                                          (import (prefix c: core^))
                                          (export viewer^)
                                          (define (add-talk-slide! s)
                                            (set! slides (cons (list s (c:get-margin)) slides))
                                            (when (and stop-after
                                                       ((length slides) . >= . stop-after))
                                              (escape (void))))
                                          (define (retract-talk-slide!)
                                            (set! slides (cdr slides)))
                                          (define (most-recent-talk-slide)
                                            (and (pair? slides) (caar slides)))
                                          (define display-progress void)
                                          (define set-init-page! void)
                                          (define set-use-background-frame! void)
                                          (define enable-click-advance! void)
                                          (define set-page-numbers-visible! void)
                                          (define add-click-region! void)
                                          (define add-interactive! void)
                                          (define (set-spotlight-style! #:size [size #f] 
                                                                        #:color [color #f])
                                            (void))
                                          (define set-allow-new-slides-after-close! void)
                                          (define (pict->pre-render-pict p) p)
                                          (define done-making-slides void))
                   CORE]))))
	(parameterize ([current-namespace ns])
	  (let/ec k
	    (set! escape k)
	    (dynamic-require `(file ,file) #f)))
	(map (lambda (s)
	       (let ([drawer (sliderec-drawer (car s))]
                     [margin (cadr s)])
		 (dc (lambda (dc x y)
		       (let-values ([(orig-xs orig-ys) (send dc get-scale)])
			 (send dc set-scale (* orig-xs xs) (* orig-ys ys))
			 (drawer dc (+ (/ x xs) margin) (+ (/ y ys) margin))
			 (send dc set-scale orig-xs orig-ys)))
		     w h 0 0)))
	     (reverse slides))))))
