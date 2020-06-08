
(module viewer scheme/base
  (require scheme/class
           scheme/unit
	   scheme/contract
	   (only-in scheme/list last)
           scheme/path
           scheme/file
	   mred
	   texpict/mrpict
	   texpict/utils
	   scheme/math
	   mrlib/include-bitmap
           racket/format
	   "sig.rkt"
	   "core.rkt"
	   "private/utils.rkt")

  (provide viewer@)

  ;; Needed for browsing
  (define original-security-guard (current-security-guard))
  (define orig-err-string-handler (error-value->string-handler))

  (define-logger slideshow)

  (define-unit viewer@
      (import (prefix config: cmdline^) core^)
      (export (rename viewer^
                      (viewer:set-use-background-frame! set-use-background-frame!)
                      (viewer:enable-click-advance! enable-click-advance!)
                      (viewer:set-page-numbers-visible! set-page-numbers-visible!)
                      (viewer:set-spotlight-style! set-spotlight-style!)
                      (viewer:set-allow-new-slides-after-close! set-allow-new-slides-after-close!)
                      (viewer:pict->pre-render-pict pict->pre-render-pict)
                      (viewer:done-making-slides done-making-slides)))

      (define-accessor margin get-margin)
      (define-accessor client-w get-client-w)
      (define-accessor client-h get-client-h)

      (define target-page config:init-page)
      (define current-page (if config:printing? config:init-page 0))
      (define use-background-frame? #f)
      (define show-page-numbers? #t)
      (define click-to-advance? #t)
      (define blank-cursor-allowed? #t)
      (define spotlight-on? #f)
      (define spotlight-shown? #f)
      (define spotlight-size 36)
      (define spotlight-color (make-color 230 230 0 0.5))
      (define click-regions null)
      (define interactives #hash())
      (define talk-slide-list null)
      (define given-talk-slide-list null)
      (define talk-slide-reverse-cell-list null)
      (define given-slide-count 0)
      (define slide-count 0)

      (define allow-more-slides? #f)
      (define error-on-slide? #f)

      (define (clear-all-slides!)
        (set! talk-slide-list null)
        (set! given-talk-slide-list null)
        (set! talk-slide-reverse-cell-list null)
        (set! slide-count 0)
        (set! given-slide-count 0)
        (set! current-page 0))

      (define empty-slide
	(make-sliderec (lambda (dc x y) (void))
		       "<Empty>"
		       #f
		       0
		       1
		       zero-inset
		       null
                       #f
                       #f))

      (define (talk-list-ref n)
	(if (n . < . slide-count)
	    (list-ref talk-slide-list n)
	    empty-slide))

      (define (mlist->list l)
        (cond
         [(null? l) null]
         [else (cons (mcar l) (mlist->list (mcdr l)))]))

      (define (given->main!)
	(if config:quad-view?
	    (begin
	      ;; WARNING: This make slide creation O(n^2) for n slides
	      (set! talk-slide-list (make-quad (mlist->list given-talk-slide-list)))
	      (set! slide-count (length talk-slide-list)))
	    (begin
	      (set! talk-slide-list (mlist->list given-talk-slide-list))
	      (set! slide-count given-slide-count))))

      (define (add-talk-slide! s)
	(when error-on-slide?
	  (error "slide window has been closed"))
	(let ([p (mcons s null)])
	  (if (null? talk-slide-reverse-cell-list)
	      (set! given-talk-slide-list p)
	      (set-mcdr! (car talk-slide-reverse-cell-list) p))
	  (set! talk-slide-reverse-cell-list (cons p talk-slide-reverse-cell-list)))
	(set! given-slide-count (add1 given-slide-count))
	(given->main!)
	(if config:printing?
	    (report-progress)
	    (begin
	      (send f slide-changed (sub1 slide-count))
	      (when (and target-page (= target-page (sub1 slide-count)))
		(set-init-page! target-page)
		(set! target-page #f))
	      (yield))))

      (define (retract-talk-slide!)
	(unless (null? talk-slide-reverse-cell-list)
	  (set! talk-slide-reverse-cell-list (cdr talk-slide-reverse-cell-list))
	  (if (null? talk-slide-reverse-cell-list)
	      (set! given-talk-slide-list null)
	      (set-mcdr! (car talk-slide-reverse-cell-list) null)))
	(set! given-slide-count (sub1 given-slide-count))
	(given->main!)
	(unless config:printing?
	  (send f slide-changed slide-count)
	  (yield)))

      (define (most-recent-talk-slide)
	(and (pair? talk-slide-reverse-cell-list)
	     (mcar (car talk-slide-reverse-cell-list))))

      (define (set-init-page! p)
	(unless config:printing?
          (set! current-page p)
          (refresh-page)))

      (define (viewer:set-use-background-frame! on?)
	(set! use-background-frame? (and on? #t)))

      (define (viewer:enable-click-advance! on?)
	(set! click-to-advance? (and on? #t)))

      (define (viewer:set-page-numbers-visible! on?)
	(set! show-page-numbers? (and on? #t)))
      (viewer:set-page-numbers-visible! config:show-page-numbers?)

      (define (viewer:set-spotlight-style! #:size [size #f]
                                           #:color [color #f])
        (when size (set! spotlight-size size))
        (when color (set! spotlight-color color)))

      (define (viewer:set-allow-new-slides-after-close! on?)
        (set! allow-more-slides? (and on? #t)))

      (define adjust-cursor (lambda () (send f set-blank-cursor #f)))

      (define (add-click-region! cr)
	(adjust-cursor)
	(set! click-regions (cons cr click-regions)))

      (define (add-interactive! ir)
        (set! interactives (hash-set interactives ir #t))
        (send c queue-interactive-check))

      (define (make-quad l)
	(cond
	 [(null? l) null]
	 [(< (length l) 4)
	  (make-quad (append l (vector->list
				(make-vector
				 (- 4 (length l))
				 (make-sliderec void #f #f
						(sliderec-page (last l))
						1
						zero-inset
						null
                                                #f
                                                #f)))))]
	 [else (let ([a (car l)]
		     [b (cadr l)]
		     [c (caddr l)]
		     [d (cadddr l)]
		     [untitled "(untitled)"])
		 (cons (make-sliderec
			(lambda (dc x y)
			  (define-values (orig-sx orig-sy) (send dc get-scale))
			  (define-values (orig-ox orig-oy) (send dc get-origin))
			  (define scale (min (/ (- (/ client-h 2) margin) client-h)
					     (/ (- (/ client-w 2) margin) client-w)))
			  (define (set-origin x y)
			    (send dc set-origin (+ orig-ox (* x orig-sx)) (+ orig-oy (* y orig-sy))))
                          (define (call-with-clipping thunk)
                            (let ([clip-rgn (send dc get-clipping-region)])
                              (send dc set-clipping-rect
                                    (- margin)
                                    (- margin)
                                    (+ client-w (* 2 margin))
                                    (+ client-h (* 2 margin)))
                              (thunk)
                              (send dc set-clipping-region clip-rgn)))
			  (send dc set-scale (* orig-sx scale) (* orig-sy scale))
			  (set-origin x y)
                          (call-with-clipping
                           (lambda ()
                             ((sliderec-drawer a) dc 0 0)))
			  (set-origin (+ x (/ client-w 2) margin) y)
                          (call-with-clipping
                           (lambda ()
                             ((sliderec-drawer b) dc 0 0)))
			  (set-origin x (+ y (/ client-h 2) margin))
                          (call-with-clipping
                           (lambda ()
                             ((sliderec-drawer c) dc 0 0)))
			  (set-origin (+ x (/ client-w 2) margin) (+ y (/ client-h 2) margin))
                          (call-with-clipping
                           (lambda ()
                             ((sliderec-drawer d) dc 0 0)))
			  (send dc set-scale orig-sx orig-sy)
			  (set-origin x y)
			  (send dc draw-line (/ client-w 2) 0 (/ client-w 2) client-h)
			  (send dc draw-line 0 (/ client-h 2) client-w (/ client-h 2))
			  (send dc set-origin orig-ox orig-oy))
			(format "~a | ~a | ~a | ~a"
				(or (sliderec-title a) untitled)
				(or (sliderec-title b) untitled)
				(or (sliderec-title c) untitled)
				(or (sliderec-title d) untitled))
			#f
			(sliderec-page a)
			(- (+ (sliderec-page d) (sliderec-page-count d)) (sliderec-page a))
			zero-inset
			null
                        (sliderec-timeout a)
                        #f)
		       (make-quad (list-tail l 4))))]))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                   Main GUI                    ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define GAUGE-WIDTH 100)
      (define GAUGE-HEIGHT 4)

      (define talk-start-seconds (current-seconds))
      (define slide-start-seconds (current-seconds))

      (define blank-cursor (make-object cursor% 'blank))

      (application-quit-handler (lambda ()
				  (send f stop-show)))

      (define current-use-screen-ws config:use-screen-ws)
      (define current-use-screen-hs config:use-screen-hs)

      (define auto-resize-frame%
        (class frame%
          (super-new)
          (inherit move resize)
          (define/augment (display-changed)
            (inner (void) display-changed)
            (when config:auto-screen-size?
              (define-values (w h) (get-display-size #t))
              (define-values (dx dy) (get-display-left-top-inset))
              (define scales (for/hasheq ([(aspect use-screen-w) (in-hash config:use-screen-ws)])
                               (define use-screen-h (hash-ref config:use-screen-hs aspect))
                               (values aspect
                                       (min (/ w use-screen-w)
                                            (/ h use-screen-h)))))
              (set!-values (current-use-screen-ws current-use-screen-hs)
                           (values
                            (for/hasheq ([(aspect use-screen-w) (in-hash config:use-screen-ws)])
                              (define s (hash-ref scales aspect))
                              (values aspect (floor (* s use-screen-w))))
                            (for/hasheq ([(aspect use-screen-h) (in-hash config:use-screen-hs)])
                              (define s (hash-ref scales aspect))
                              (values aspect (floor (* s use-screen-h))))))
              (move (- dx) (- dy))
              (resize w h)))))

      (define talk-frame%
	(class auto-resize-frame%
	  (init-field closeable?)
	  (init-field close-bg?)
	  (define/augment can-close? (lambda () (and closeable? (inner #t can-close?))))
	  (define/override on-superwindow-show (lambda (on?)
						 (unless on?
						   (when (and close-bg? background-f)
						     (send background-f show #f))
                                             (stop-time-update!)
                                             (stop-elapsed-timer!))))

	  (define/override on-subwindow-char
	    (lambda (w e)
	      (let ([k (send e get-key-code)])
		(case k
		  [(right)
		   (shift e 1 0 (lambda () (next)))]
		  [(left)
		   (shift e -1 0 (lambda () (prev)))]
		  [(up)
		   (shift e 0 -1 (lambda () (prev)))]
		  [(down)
		   (shift e 0 1 (lambda () (next)))]
		  [(#\space #\f #\n next)
		   (next)
		   #t]
		  [(#\b #\backspace #\rubout prior)
		   (prev)
		   #t]
                  [(#\s)
                   (next-title add1 #f)]
                  [(#\a)
                   (next-title sub1 #t)]
		  [(#\g)
		   (stop-transition)
		   (send f set-blank-cursor #f)
		   (if (send e get-meta-down)
		       (get-page-from-user)
		       (begin
			 (set! current-page (max 0 (sub1 slide-count)))
			 (refresh-page)))
		   (send f set-blank-cursor blank-cursor-allowed?)
		   #t]
		  [(#\1)
		   (stop-transition)
		   (set! current-page 0)
		   (refresh-page)
		   #t]
		  [(#\q #\u0153)  ; #\u0153 is for Mac OS
		   (stop-transition)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (stop-show))
		   #t]
		  [(escape)
		   (send f set-blank-cursor #f)
                   (define result
                     (send c
                           call-with-suspended-interactive
                           (lambda ()
                             (message-box/custom
                              "Quit"
                              "Really quit the slide show?"
                              "&Quit"
                              "&Cancel"
                              #f
                              this
                              '(default=1 caution)))))
		   (when (equal? 1 result)
		     (stop-show))
		   (send f set-blank-cursor blank-cursor-allowed?)
		   #t]
		  [(#\p)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (set! show-page-numbers? (not show-page-numbers?))
		     (stop-transition)
		     (refresh-page))
		   #t]
		  [(#\d)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (stop-transition)
		     (send f-both show (not (send f-both is-shown?)))
		     (refresh-page))
		   #t]
		  [(#\c)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (stop-transition)
		     (send c-frame show (not (send c-frame is-shown?))))
		   #t]
		  [(#\m)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (set! blank-cursor-allowed? (not blank-cursor-allowed?))
		     (send f set-blank-cursor blank-cursor-allowed?))]
		  [(#\l)
		   (when (or (send e get-meta-down)
			     (send e get-alt-down))
		     (set! spotlight-on? (not spotlight-on?))
                     (when spotlight-on?
                       (set! spotlight-shown? #t))
                     (send c refresh))]
		  [else
		   #f]))))

	  (define/public (stop-show)
	    (send c-frame show #f)
	    (send f-both show #f)
	    (when use-background-frame?
	      (send f show #f))
           (stop-time-update!)
           (stop-elapsed-timer!)
	    (send f show #f)
	    (when config:print-slide-seconds?
	      (printf "Total Time: ~a seconds\n"
		      (- (current-seconds) talk-start-seconds)))
            (if allow-more-slides?
                (clear-all-slides!)
                ;; In case slides are still building, tell them to stop. We
                ;;  prefer not to `exit' directly if we don't have to.
                (set! error-on-slide? #t)))

	  (define/private (shift e xs ys otherwise)
	    (cond
	     [(or (send e get-meta-down)
		  (send e get-alt-down))
	      (move-over (* xs 20) (* ys 20))]
	     [(send e get-shift-down)
	      (move-over xs ys)]
	     [else
	      (otherwise)])
	    #t)

	  (inherit get-x get-y move)
	  (define/private (move-over dx dy)
	    (let ([x (get-x)]
		  [y (get-y)])
	      (move (+ x dx) (+ y dy)))
	    (when background-f
	      (let ([x (send background-f get-x)]
		    [y (send background-f get-y)])
		(send background-f move (+ x dx) (+ y dy)))))

	  (define/public (prev)
	    (stop-transition)
	    (set! current-page
                  (let loop ([pos (max (sub1 current-page) 0)])
                    (cond
                     [(zero? pos) pos]
                     [(sliderec-timeout (talk-list-ref pos)) (loop (sub1 pos))]
                     [else pos])))
	    (refresh-page))

	  (define/public (next)
	    (if (pair? current-transitions)
		(stop-transition)
                (if (sliderec-timeout (talk-list-ref current-page))
                    ;; skip to a slide without a timeout:
                    (change-slide
                     (- (let loop ([pos (add1 current-page)])
                          (cond
                           [(= pos slide-count) (sub1 slide-count)]
                           [(sliderec-timeout (talk-list-ref pos)) (loop (add1 pos))]
                           [else pos]))
                        current-page))
                    ;; normal step:
                    (change-slide 1))))

          (define/public (next-one)
            (if (pair? current-transitions)
		(stop-transition)
                (change-slide 1)))

          (define/public (next-title d1 far-end?)
            ;; skip to next slide that has a different title
            (stop-transition)
            (define current-title (sliderec-title (talk-list-ref current-page)))
            (change-slide
             (- (let loop ([pos (d1 current-page)])
                  (cond
                   [(negative? pos) 0]
                   [(= pos slide-count) (sub1 slide-count)]
                   [(equal? current-title (sliderec-title (talk-list-ref pos))) (loop (d1 pos))]
                   [far-end?
                    (define new-title (sliderec-title (talk-list-ref pos)))
                    (let loop ([prev-pos pos] [pos (d1 pos)])
                      (cond
                       [(negative? pos) 0]
                       [(= pos slide-count) (sub1 slide-count)]
                       [(equal? new-title (sliderec-title (talk-list-ref pos))) (loop pos (d1 pos))]
                       [else prev-pos]))]
                   [else pos]))
                current-page)))

	  (define/public (slide-changed pos)
	    (when (or (= pos current-page)
		      (and (or config:use-prefetch?
			       (send f-both is-shown?))
			   (= pos (add1 current-page))))
	      (stop-transition)
	      (set! prefetched-page #f)
	      (change-slide 0)
	      (when (and (= pos 0)
			 (not config:printing?))
		(when use-background-frame?
		  (send f show #f)
		  (yield)
		  (send background-f show #t))
                (start-time-update!)
		(send f show #t)
		(when config:two-frames?
		  (send f-both show #t)))))

	  (define/private (change-slide n)
	    (let ([old (talk-list-ref current-page)])
	      (set! current-page (max 0
				      (min (+ n current-page)
					   (sub1 slide-count))))
	      (when config:print-slide-seconds?
		(let ([slide-end-seconds (current-seconds)])
		  (printf "Slide ~a: ~a seconds\n" current-page
			  (- slide-end-seconds slide-start-seconds))
		  (set! slide-start-seconds slide-end-seconds)))
	      ;; Refresh screen, and start transitions from old, if any
	      (do-transitions (if config:use-transitions?
				  (sliderec-transitions old)
				  null)
			      (send c get-offscreen))))

	  (define blank-cursor? #f)
	  (define activated? #f)

	  (inherit set-cursor)

	  (define/override (on-activate on?)
	    (set! activated? on?)
	    (when blank-cursor?
	      (set-cursor (if (and blank-cursor? on? blank-cursor-allowed?)
			      blank-cursor
			      #f))))
	  (define/public (set-blank-cursor b?)
	    (set! blank-cursor? (and b? #t))
	    (when activated?
	      (set-cursor (if (and blank-cursor? blank-cursor-allowed?)
			      blank-cursor
			      #f))))

          (define/augment (display-changed)
            (when config:auto-screen-size?
              (set! prefetched-page #f))
            (inner (void) display-changed))

	  (super-new)))

      (define-values (screen-left-inset screen-top-inset)
	(if config:keep-titlebar?
	    (values 0 0)
	    (get-display-left-top-inset #:monitor config:screen-number)))

      (when config:right-half-screen?
        (set! screen-left-inset (- screen-left-inset config:actual-screen-w)))

      (define fullscreen?
        (and (not config:keep-titlebar?)
             (let-values ([(w h) (get-display-size #t #:monitor config:screen-number)])
               (and (= config:actual-screen-w w)
                    (= config:actual-screen-h h)))))

      (define background-f
	(make-object (class auto-resize-frame%
		       (inherit is-shown?)
		       (define/override (on-activate on?)
			 (when (and on? (is-shown?))
			   (send f show #t)))
		       (super-new
			[label "Slideshow Background"]
			[x (- screen-left-inset)] [y (- screen-top-inset)]
			[width (inexact->exact (floor config:actual-screen-w))]
			[height (inexact->exact (floor config:actual-screen-h))]
			[style (append
                                (if fullscreen? '(hide-menu-bar) null)
                                '(no-caption no-resize-border))]))))

      (send background-f enable #f)

      (define f (new talk-frame%
		     [closeable? config:keep-titlebar?]
		     [close-bg? #t]
		     [label (if config:file-to-load
				(format "~a: slideshow" (file-name-from-path config:file-to-load))
				"Slideshow")]
		     [x (- screen-left-inset)] [y (- screen-top-inset)]
		     [width (inexact->exact (floor config:actual-screen-w))]
		     [height (inexact->exact (floor config:actual-screen-h))]
		     [style (if config:keep-titlebar?
				null
                                (append
                                 (if fullscreen? '(hide-menu-bar) null)
                                 '(no-caption no-resize-border)))]))

      (define f-both (new talk-frame%
			  [closeable? #t]
			  [close-bg? #f]
			  [label "Slideshow Preview"]
			  [x 0] [y 0]
			  [width (inexact->exact (floor (* config:actual-screen-w 8/10 1)))]
			  [height (inexact->exact (floor (* config:actual-screen-h 8/10 2/3)))]
			  [style '()]))

      (define current-sinset zero-inset)
      (define resizing-frame? #f)
      (define (reset-display-inset! sinset dc aspect)
	(unless (and (= (sinset-l current-sinset) (sinset-l sinset))
		     (= (sinset-t current-sinset) (sinset-t sinset))
		     (= (sinset-r current-sinset) (sinset-r sinset))
		     (= (sinset-b current-sinset) (sinset-b sinset)))
	  (set! resizing-frame? #t) ; hack --- see yield below
          (send dc clear)
	  (send f resize
		(max 1 (- (inexact->exact (floor config:actual-screen-w))
			  (inexact->exact (floor (* (+ (sinset-l sinset) (sinset-r sinset))
						    (/ config:actual-screen-w
                                                       (hash-ref config:screen-ws aspect)))))))
		(max 1 (- (inexact->exact (floor config:actual-screen-h))
			  (inexact->exact (floor (* (+ (sinset-t sinset) (sinset-b sinset))
						    (/ config:actual-screen-h
                                                       (hash-ref config:screen-hs aspect))))))))
	  (send f move
		(inexact->exact (- (floor (* (sinset-l sinset)
					     (/ config:actual-screen-w
                                                (hash-ref config:screen-ws aspect))))
				   screen-left-inset))
		(inexact->exact (- (floor (* (sinset-t sinset)
					     (/ config:actual-screen-h
                                                (hash-ref config:screen-hs aspect))))
				   screen-top-inset)))
	  (set! current-sinset sinset)
	  ;; FIXME: This yield is here so that the frame
	  ;;  and its children can learn about their new
	  ;;  sizes, and so that the generated on-size callback
	  ;;  can be ignored. Obviously, using yield creates a
	  ;;  kind of race condition for incoming events from the user.
	  (let loop () (when (yield) (loop)))
	  (set! resizing-frame? #f)))


      (define c-frame (new (class talk-frame%
			     (define/override (on-move x y)
			       (super on-move x y)
			       (parameterize ([current-security-guard original-security-guard]
                                              [error-value->string-handler orig-err-string-handler])
				 (with-handlers ([void void]) ; also prevents exn handler from grabbing security guard
				   (put-preferences '(slideshow:commentary-x slideshow:commentary-y)
						    (list x y)
						    void))))
			     (define/override (on-size w h)
			       (super on-size w h)
			       (parameterize ([current-security-guard original-security-guard]
                                              [error-value->string-handler orig-err-string-handler])
				 (with-handlers ([void void]) ; also prevents exn handler from grabbing security guard
				   (put-preferences '(slideshow:commentary-width slideshow:commentary-height)
						    (list w h)
						    void))))
			     (super-new))
			   [closeable? #t]
			   [close-bg? #f]
			   [label "Commentary"]
			   [width (get-preference 'slideshow:commentary-width (lambda () 400))]
			   [height (get-preference 'slideshow:commentary-height (lambda () 100))]
			   [x (get-preference 'slideshow:commentary-x (lambda () #f))]
			   [y (get-preference 'slideshow:commentary-y (lambda () #f))]))
      (define commentary (make-object text%))
      (send (new (class editor-canvas%
		   (define/override (on-event e)
		     (super on-event e)
		     (when click-to-advance?
                       (when (send e button-up? 'left)
                         (send f next))
                       (when (send e button-up? 'right)
                         (send f prev))))
		   (super-new))
		 [parent c-frame]
		 [editor commentary]
		 [style (if (eq? (system-type) 'macosx)
			    '(auto-hscroll resize-corner)
			    '(auto-hscroll auto-vscroll))])
	    set-line-count 3)
      (send commentary auto-wrap #t)
      (send c-frame reflow-container)
      (define SCROLL-STEP-SIZE 20)
      (define pict-snip%
	(class snip%
	  (init-field pict)
	  (define drawer (make-pict-drawer pict))
	  (define/override (draw dc x y left top right bottom dx dy draw-caret)
	    (drawer dc x y))
	  (define/private (set-box/f b v)
	    (when b (set-box! b v)))
	  (define/override (get-extent dc x y wbox hbox descent space lspace rspace)
	    (set-box/f wbox (pict-width pict))
	    (set-box/f hbox (pict-height pict))
	    (set-box/f descent (pict-descent pict))
	    (set-box/f space 0)
	    (set-box/f lspace 0)
	    (set-box/f rspace 0))
	  (define/override (get-num-scroll-steps)
	    (inexact->exact (ceiling (/ (pict-height pict) SCROLL-STEP-SIZE))))
	  (define/override (find-scroll-step y)
	    (inexact->exact (floor (/ (max 0 y) SCROLL-STEP-SIZE))))
	  (define/override (get-scroll-step-offset n)
	    (* n SCROLL-STEP-SIZE))
	  (super-new)
	  (inherit set-snipclass)
	  (set-snipclass pict-snipclass)))
      (define pict-snipclass (new snip-class%))


      (define start-time #f)

      (define clear-brush (make-object brush% "WHITE" 'transparent))
      (define white-brush (make-object brush% "WHITE" 'solid))
      (define gray-brush (make-object brush% "GRAY" 'solid))
      (define green-brush (make-object brush% "GREEN" 'solid))
      (define red-brush (make-object brush% "RED" 'solid))
      (define letterbox-brush (make-object brush% config:letterbox-color 'solid))
      (define black-pen (make-object pen% "BLACK" 1 'solid))
      (define clear-pen (make-object pen% "BLACK" 1 'transparent))
      (define red-color (make-object color% "RED"))
      (define green-color (make-object color% "GREEN"))
      (define black-color (make-object color% "BLACK"))

      (define (slide-page-string slide)
        (let ([s ((current-page-number-adjust)
                  (sliderec-page slide)
                  (if (= 1 (sliderec-page-count slide))
                      (format "~a" (sliderec-page slide))
                      (format "~a-~a" (sliderec-page slide) (+ (sliderec-page slide)
                                                               (sliderec-page-count slide)
                                                               -1))))])
          (unless (string? s)
            (error 'current-page-number-adjust "expected a procedure that returned a string, but it returned ~s" s))
          s))

      (define (calc-progress)
	(if (and start-time config:talk-duration-minutes)
	    (values (min 1 (/ (- (current-seconds) start-time) (* 60 config:talk-duration-minutes)))
		    (/ current-page (max 1 (sub1 slide-count))))
	    (values 0 0)))

      (define c%
	(class canvas%
	  (inherit get-dc get-client-size make-bitmap
                   client->screen refresh)

	  (define clicking #f)
	  (define clicking-hit? #f)

          (define mouse-x 0)
          (define mouse-y 0)

	  (define/override (on-paint)
	    (let ([dc (get-dc)])
	      (stop-transition/no-refresh)
	      (cond
	       [config:use-offscreen?
		(let ([bm (send offscreen get-bitmap)])
		  (send (get-dc) draw-bitmap bm 0 0))]
	       [else
		(send dc clear)
		(paint-slide this dc)])
              (show-spotlight dc)
              (show-time dc (sliderec-aspect (talk-list-ref current-page)))))

          (define/private (show-spotlight dc)
            (when (and spotlight-on? spotlight-shown?)
              (define old-p (send dc get-pen))
              (define old-b (send dc get-brush))
              (send dc set-pen "black" 0 'transparent)
              (send dc set-brush spotlight-color 'solid)
              (send dc draw-ellipse
                    (- mouse-x (/ spotlight-size 2))
                    (- mouse-y (/ spotlight-size 2))
                    spotlight-size
                    spotlight-size)
              (send dc set-pen old-p)
              (send dc set-brush old-b)))

          (define/private (show-time dc aspect)
            (when config:show-time?
              (define c (send dc get-text-foreground))
              (define f (send dc get-font))
              (define time-size 10)
              (send dc set-text-foreground (make-color 100 100 100))
              (send dc set-font (make-font #:size time-size #:size-in-pixels? #t))
              (let-values ([(cw ch) (get-client-size)])
                (let ([dx (floor (/ (- cw (hash-ref current-use-screen-ws aspect)) 2))]
                      [dy (floor (/ (- ch (hash-ref current-use-screen-hs aspect)) 2))]
                      [d (seconds->date (current-seconds))])
                  (send dc draw-text
                        (~a (let ([h (modulo (date-hour d) 12)])
                              (if (zero? h) 12 h))
                            ":"
                            (~a #:width 2 #:align 'right #:pad-string "0"
                                (date-minute d)))
                        (+ dx 5)
                        (+ dy (- (hash-ref current-use-screen-hs aspect) time-size 5)))))
              (send dc set-text-foreground c)
              (send dc set-font f)))

	  (inherit get-top-level-window)
	  (define/override (on-event e)
            (unless (and (= mouse-x (send e get-x))
                         (= mouse-y (send e get-y)))
              (set! mouse-x (send e get-x))
              (set! mouse-y (send e get-y))
              (when spotlight-on?
                (set! spotlight-shown? #t)
                (refresh)))
	    (cond
	     [(send e button-down?)
	      (let ([c (ormap
			(lambda (c) (and (click-hits? e c) c))
			click-regions)])
		(when c
		  (if (click-region-show-click? c)
		      (begin
			(set! clicking c)
			(set! clicking-hit? #t)
			(invert-clicking! #t))
		      ((click-region-thunk c)))))]
	     [(and clicking (send e dragging?))
	      (let ([hit? (click-hits? e clicking)])
		(unless (eq? hit? clicking-hit?)
		  (set! clicking-hit? hit?)
		  (invert-clicking! hit?)))]
	     [(and clicking (send e button-up?))
	      (let ([hit? (click-hits? e clicking)]
		    [c clicking])
		(unless (eq? hit? clicking-hit?)
		  (set! clicking-hit? hit?)
		  (invert-clicking! hit?))
		(when clicking-hit?
		  (invert-clicking! #f))
		(set! clicking #f)
		(when hit?
		  ((click-region-thunk c))))]
	     [(send e button-up? 'left)
	      (when click-to-advance?
		(send (get-top-level-window) next))]
	     [(send e button-up? 'right)
	      (when click-to-advance?
		(send (get-top-level-window) prev))]
	     [else
	      (when (and clicking clicking-hit?)
		(invert-clicking! #f))
	      (set! clicking #f)]))


	  (define/private (click-hits? e c)
	    (let ([x (send e get-x)]
		  [y (send e get-y)])
	      (and (<= (click-region-left c) x (click-region-right c))
		   (<= (click-region-top c) y (click-region-bottom c)))))
	  (define/private (invert-clicking! on?)
	    (let ([dc (get-dc)]
		  [x (click-region-left clicking)]
		  [y (click-region-top clicking)]
		  [w (- (click-region-right clicking) (click-region-left clicking))]
		  [h (- (click-region-bottom clicking) (click-region-top clicking))])
              (let ([x (floor x)]
                    [y (floor y)]
                    [w (- (floor (+ x w)) (floor x))]
                    [h (- (floor (+ y h)) (floor y))])
                (if (or on?
                        (not config:use-offscreen?)
                        (not offscreen))
                    (let* ([b (send dc get-brush)]
                           [p (send dc get-pen)])
                      (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
                      (send dc set-brush  (send the-brush-list find-or-create-brush "black"
                                                (if config:use-offscreen?
                                                    'hilite
                                                    'xor)))
                      (send dc draw-rectangle x y w h)
                      (send dc set-pen p)
                      (send dc set-brush b))
                    (send dc draw-bitmap-section
                          (send offscreen get-bitmap)
                          x y x y
                          w h)))))

	  (define offscreen #f)
	  (define/public get-offscreen (lambda () offscreen))

	  (define/private (shift-click-region cr dx dy)
	    (make-click-region (+ dx (click-region-left cr))
			       (+ dy (click-region-top cr))
			       (+ dx (click-region-right cr))
			       (+ dy (click-region-bottom cr))
			       (click-region-thunk cr)
			       (click-region-show-click? cr)))

	  (define/private (shift-interact cr dx dy)
	    (make-interact (+ dx (interact-left cr))
                           (+ dy (interact-top cr))
                           (+ dx (interact-right cr))
                           (+ dy (interact-bottom cr))
                           (interact-proc cr)))

	  (define/private (paint-prefetch dc)
	    (let-values ([(cw ch) (get-client-size)])
              (paint-letterbox dc cw ch
                               (hash-ref current-use-screen-ws prefetch-aspect)
                               (hash-ref current-use-screen-hs prefetch-aspect)
                               #f)
	      (let ([dx (floor (/ (- cw (hash-ref current-use-screen-ws prefetch-aspect)) 2))]
		    [dy (floor (/ (- ch (hash-ref current-use-screen-hs prefetch-aspect)) 2))])
		(send dc draw-bitmap prefetch-bitmap dx dy)
		(set! click-regions (map (lambda (cr)
					   (shift-click-region cr dx dy))
					 prefetched-click-regions))
                (set! interactives (for/hash ([k (in-hash-keys prefetched-interactives)])
                                     (values (shift-interact k dx dy)
                                             #t)))
                (when (positive? (hash-count prefetched-interactives))
                  (queue-interactive-check))
		(send f set-blank-cursor (null? click-regions)))))
	  (define/override (on-size w h)
	    (unless resizing-frame?
	      (redraw)))

	  (define/public (redraw)
	    (unless printing?
              (define now (current-milliseconds))
              (define slide (talk-list-ref current-page))
	      (reset-display-inset! (sliderec-inset slide) (get-dc) (sliderec-aspect slide))
	      (send commentary lock #f)
	      (send commentary begin-edit-sequence)
	      (send commentary erase)
	      (let ([s (talk-list-ref current-page)])
		(when (just-a-comment? (sliderec-comment s))
		  (for-each (lambda (v)
			      (send commentary insert (if (string? v)
							  v
							  (make-object pict-snip% v))))
			    (just-a-comment-content (sliderec-comment s)))))
	      (send commentary scroll-to-position 0 #f 'same 'start)
	      (send commentary end-edit-sequence)
	      (send commentary lock #t)
	      (set! click-regions null)
              (define old-interactives interactives)
	      (set! interactives #hash())
	      (set! clicking #f)
	      (stop-transition/no-refresh)
              (when (sliderec-timeout (talk-list-ref current-page))
                (let ([key (gensym)])
                  (set! current-timeout-key key)
                  (define interval
                    (inexact->exact
                     (floor
                      (* (sliderec-timeout (talk-list-ref current-page))
                         1000))))
                  (new timer%
                       [interval interval]
                       [just-once? #t]
                       [notify-callback
                        (lambda ()
                          (when (eq? current-timeout-key key)
			    ;; run as low priority:
			    (queue-callback
			     (lambda ()
                               (collect-garbage 'incremental)
                               (when (send f is-shown?)
                                 (log-slideshow-debug "Timeout vs. requested: ~s vs. ~s"
                                                      (- (current-milliseconds) now)
                                                      interval)
                                 (send c-frame next-one)))
			     #f)))])))
	      (cond
	       [config:use-offscreen?
		(let-values ([(cw ch) (get-client-size)])
		  (when (and offscreen
			     (let ([bm (send offscreen get-bitmap)])
			       (not (and (= cw (send bm get-width))
					 (= ch (send bm get-height))))))
		    (send offscreen set-bitmap #f)
		    (set! offscreen #f))
		  (unless offscreen
		    (set! offscreen (make-object bitmap-dc%
						 (make-bitmap cw ch)))))
		(send offscreen clear)
		(cond
		 [(equal? prefetched-page current-page)
		  (paint-prefetch offscreen)]
		 [else
		  (paint-slide this offscreen)])
		(let ([bm (send offscreen get-bitmap)])
		  (send (get-dc) draw-bitmap bm 0 0))]
	       [(equal? prefetched-page current-page)
		(paint-prefetch (get-dc))]
	       [else
		(let ([dc (get-dc)])
		  (send dc clear)
		  (paint-slide this dc))])
              (show-spotlight (get-dc))
              (show-time (get-dc) (sliderec-aspect slide))
              (swap-interactives! old-interactives interactives)
              (log-slideshow-debug "Redraw time: ~s"
                                   (- (current-milliseconds) now))))

          (define interactive-state (make-hash))
          (define/private (swap-interactives! old new-i)
            (define shared (for/hash ([k (in-hash-keys old)]
                                      #:when (hash-ref new-i k #f))
                             (values k #t)))
            (for ([k (in-hash-keys old)])
              (unless (hash-ref shared k #f)
                (define proc+win (hash-ref interactive-state k #f))
                (when proc+win
                  (hash-remove! interactive-state k)
                  ((car proc+win))
                  (send (cdr proc+win) show #f))))
            (for ([k (in-hash-keys new-i)])
              (unless (hash-ref shared k #f)
                (define win (new frame%
                                 [label "Interactive"]
                                 [style '(float no-resize-border no-caption no-system-menu)]))
                (define-values (left top) (client->screen (inexact->exact (floor (interact-left k)))
                                                          (inexact->exact (floor (interact-top k)))))
                (define-values (right bottom) (client->screen (inexact->exact (ceiling (interact-right k)))
                                                              (inexact->exact (ceiling (interact-bottom k)))))
                (define-values (dx dy) (get-display-left-top-inset))
                (send win move (- left dx) (- top dy))
                (send win resize (- right left) (- bottom top))
                (define cancel ((interact-proc k) win))
                (hash-set! interactive-state k (cons cancel win))
                (send win show #t)))
            (set! interactives new-i))

          (define/override on-superwindow-show
            (lambda (on?)
              (unless on?
                (swap-interactives! interactives #hash()))))

          (define/public (queue-interactive-check)
            ;; If a superwindow is hidden before we ere get a notification
            ;; that the superwindow was visible, then we don't get a
            ;; notification that the window is hidden. Use a low-priority
            ;; callback to double-check.
            (queue-callback (lambda ()
                              (when (positive? (hash-count interactives))
                                (let loop ([p this])
                                  (cond
                                   [(not p) (void)]
                                   [(send p is-shown?) (loop (send p get-parent))]
                                   [else
                                    ;; some superwindow is hidden
                                    (swap-interactives! interactives #hash())]))))
                            #f))

          (define/public (call-with-suspended-interactive thunk)
            (define i interactives)
            (swap-interactives! i #hash())
            (begin0
             (thunk)
             (swap-interactives! #hash() i)))

          (super-new [style '(no-autoclear)])))

      (define two-c%
	(class canvas%
	  (inherit get-dc)

	  (define/public (paint-prefetched)
	    (let ([dc (get-dc)])
	      (let*-values ([(cw ch) (send dc get-size)])
		(send dc set-scale
		      (/ (* cw 1/2) (send prefetch-bitmap get-width))
		      (/ ch (send prefetch-bitmap get-height)))
		(send dc set-origin (/ cw 2) 0)
		(send dc draw-bitmap prefetch-bitmap 0 0)
		(send dc set-origin 0 0)
		(send dc set-scale 1 1)
		(send dc draw-line (/ cw 2) 0 (/ cw 2) ch))))

         (define/private (show-elapsed-time)
           (define dc (get-dc))
           (define-values (cw ch) (send dc get-size))
           (let* ([text-fg (send dc get-text-foreground)]
                  [text-bg (send dc get-text-background)]
                  [font (send dc get-font)]
                  [elapsed (- (current-seconds) talk-start-seconds)]
                  [etime (seconds->hhmmss elapsed)])
             (send dc set-font (make-font #:size 32 #:size-in-pixels? #t))
             (define-values (tw th bh eh) (send dc get-text-extent etime))

             (send dc set-text-foreground red-color)
             (send dc draw-text etime
                   (- (* cw 5/6) (/ tw 2))
                   (+ (* ch 5/6) (/ th 2) bh))

             (send dc set-text-foreground text-fg)
             (send dc set-text-background text-bg)
             (send dc set-font font)))

	  (define/override (on-paint)
	    (let ([dc (get-dc)])
	      (send dc clear)
	      (let*-values ([(cw ch) (send dc get-size)])
		(cond
		 [(and config:use-prefetch? config:use-prefetch-in-preview?)
		  (let* ([now-bm (send (send c get-offscreen) get-bitmap)]
			 [bw (send now-bm get-width)]
			 [bh (send now-bm get-height)])
		    (send dc set-scale (/ (/ cw 2) bw) (/ ch bh))
		    (send dc draw-bitmap now-bm 0 0)
		    (cond
		     [(equal? prefetched-page (add1 current-page))
		      (send dc set-origin (/ cw 3) 0)
		      (send dc draw-bitmap prefetch-bitmap 0 0)]
		     [else
		      (when (< (add1 current-page) slide-count)
			(let ([b (send dc get-brush)])
			  (send dc set-brush gray-brush)
			  (send dc draw-rectangle bw 0 bw bh)
			  (send dc set-brush b)))])
		    (send dc set-scale 1 1))]
		 [else
		  (paint-slide this dc current-page 2/3 1 cw ch cw ch #f)
		  (let ([pen (send dc get-pen)]
			[brush (send dc get-brush)])
		    (send dc set-pen "black" 1 'solid)
		    (send dc set-brush "black" 'solid)
		    (send dc draw-rectangle
			  (* cw 2/3)
			  0
			  (* cw 1/3)
			  (* ch 1/6))
		    (send dc draw-rectangle
			  (* cw 2/3)
			  (* ch 5/6)
			  (* cw 1/3)
			  (* ch 1/6))
		    (send dc set-pen pen)
		    (send dc set-brush brush))
		  (send dc set-origin (* cw 2/3) (* ch 1/6))
		  (when (< (add1 current-page) slide-count)
		    (send dc draw-rectangle (* cw 2/3) 0 (* cw 1/3) ch)
                    (paint-slide this
                                 dc
				 (+ current-page 1)
				 1/3 1/2
				 cw ch cw ch
				 #f))])

              (send dc set-origin 0 0)

              (when config:show-elapsed-time?
                (show-elapsed-time))

		(send dc draw-line (* cw 2/3) 0 (* cw 2/3) ch))))

	  (inherit get-top-level-window)
	  (define/override (on-event e)
	    (cond
	     [(send e button-up?)
	      (send (get-top-level-window) next)]))

	  (define/public (redraw) (unless printing? (on-paint)))
	  (super-new)))

      (define elapsed-timer-tick
        (when config:show-elapsed-time?
            (new timer%
                 [notify-callback (lambda ()
                                    (when (send c-both is-shown?)
                                      (send c-both redraw)))]
                 [interval 1000])))

      (define (stop-elapsed-timer!)
        (when config:show-elapsed-time?
          (send elapsed-timer-tick stop)))

      (define (paint-letterbox dc cw ch usw ush clip?)
	(and (or (< usw cw)
                 (< ush ch))
             (let ([b (send dc get-brush)]
                   [p (send dc get-pen)])
               (send dc set-brush letterbox-brush)
               (send dc set-pen clear-pen)
               (when (< usw cw)
                 (let ([half (/ (- cw usw) 2)])
                   (send dc draw-rectangle 0 0 half ch)
                   (send dc draw-rectangle (- cw half) 0 half ch)))
               (when (< ush ch)
                 (let ([half (/ (- ch ush) 2)])
                   (send dc draw-rectangle 0 0 cw half)
                   (send dc draw-rectangle 0 (- ch half) cw half)))
               (send dc set-brush b)
               (send dc set-pen p)
               (and clip?
                    (begin0
                     (box (send dc get-clipping-region))
                     (send dc set-clipping-rect
                           (/ (- cw usw) 2)
                           (/ (- ch ush) 2)
                           usw ush))))))

    (define (maybe-set-smoothing dc)
      (when config:smoothing?
        ; When printing, there is no known pixel grid, so 'aligned drawing
        ; doesn't make sense; see racket/draw#26.
        (send dc set-smoothing (if config:printing? 'smoothed 'aligned))))

      (define paint-slide
	(case-lambda
	 [(canvas dc) (paint-slide canvas dc current-page)]
	 [(canvas dc page)
	  (let-values ([(cw ch) (send dc get-size)])
            (define slide (if (sliderec? page) page (talk-list-ref page)))
            (define aspect (sliderec-aspect slide))
	    (paint-slide canvas dc page 1 1 cw ch
                         (hash-ref current-use-screen-ws aspect) (hash-ref current-use-screen-hs aspect)
                         #t))]
	 [(canvas dc page extra-scale-x extra-scale-y cw ch usw ush to-main?)
	  (let* ([slide (if (sliderec? page)
                            page
                            (talk-list-ref page))]
		 [ins (sliderec-inset slide)]
                 [aspect (sliderec-aspect slide)]
		 [cw (if to-main?
			 (+ cw (sinset-l ins) (sinset-r ins))
			 cw)]
		 [ch (if to-main?
			 (+ ch (sinset-t ins) (sinset-b ins))
			 ch)]
		 [sx (/ usw (hash-ref config:screen-ws aspect))]
		 [sy (/ ush (hash-ref config:screen-hs aspect))]
		 [mx (/ (- cw usw) 2)]
		 [my (/ (- ch ush) 2)])
	    (define clip-rgn (paint-letterbox dc cw ch usw ush #t))

            (maybe-set-smoothing dc)
            (send dc set-scale (* extra-scale-x sx) (* extra-scale-y sy))

	    ;; Draw the slide
	    ;;  It's important to set the origin based on
	    ;;  the floor of my and mx. That way, when we pre-fetch
	    ;;  into a bitmap, we don't change roundoff in
	    ;;  the drawing
	    (let-values ([(ox oy) (send dc get-origin)])
	      (send dc set-origin
		    (+ ox (* extra-scale-x (floor mx)))
		    (+ oy (* extra-scale-y (floor my))))
	      ((sliderec-drawer slide) dc margin margin)
	      (send dc set-origin ox oy))

	    ;; reset the scale
	    (send dc set-scale 1 1)

	    ;; Slide number
	    (when (and to-main? show-page-numbers?)
	      (let ([f (send dc get-font)]
		    [s (slide-page-string slide)]
		    [c (send dc get-text-foreground)])
		(send dc set-font (current-page-number-font))
		(send dc set-text-foreground (current-page-number-color))
		(let-values ([(w h d a) (send dc get-text-extent s)])
		  (send dc draw-text s
			(- cw w 5 (* sx (sinset-r ins)) (/ (- cw usw) 2))
			(- ch h 5 (* sy (sinset-b ins)) (/ (- ch ush) 2))))
		(send dc set-text-foreground c)
		(send dc set-font f)))

            (when clip-rgn
              (send dc set-clipping-region (unbox clip-rgn))))]))

      ;; prefetched-page : (union #f number)
      (define prefetched-page #f)
      ;; prefetch-bitmaps : aspect -> (union #f bitmap)
      (define prefetch-bitmaps #hasheq())
      ;; prefetch-bitmap : (union #f bitmap)
      (define prefetch-bitmap #f)
      ;; prefetch-dc : (union #f bitmap-dc)
      (define prefetch-dc #f)
      ;; prefetch-schedule-cancel-box : (box boolean)
      (define prefetch-schedule-cancel-box (box #f))
      ;; prefetched-click-regions : list
      (define prefetched-click-regions null)
      ;; prefetched-interactives : hash
      (define prefetched-interactives #hash())
      (define prefetch-aspect #f)

      (define (prefetch-slide canvas n)
	(set! prefetched-page #f)

        (define slide (if (sliderec? n) n (talk-list-ref n)))
        (define aspect (sliderec-aspect slide))
        (define current-use-screen-w (hash-ref current-use-screen-ws aspect))
        (define current-use-screen-h (hash-ref current-use-screen-hs aspect))

	(unless prefetch-dc
	  (set! prefetch-dc (new bitmap-dc%)))

        (set! prefetch-aspect aspect)
        (set! prefetch-bitmap (hash-ref prefetch-bitmaps aspect #f))
          
	;; try to re-use existing bitmap
	(unless (and (is-a? prefetch-bitmap bitmap%)
		     (= current-use-screen-w (send prefetch-bitmap get-width))
		     (= current-use-screen-h (send prefetch-bitmap get-height)))
	  (send prefetch-dc set-bitmap #f)
	  (set! prefetch-bitmap (send canvas make-bitmap current-use-screen-w current-use-screen-h))
          (set! prefetch-bitmaps (hash-set prefetch-bitmaps aspect prefetch-bitmap))
	  (when (send prefetch-bitmap ok?)
	    (send prefetch-dc set-bitmap prefetch-bitmap)))

	(when (send prefetch-dc ok?)
	  (send prefetch-dc clear)
	  (let ([old-click-regions click-regions]
                [old-interactives interactives]
		[old-adjust adjust-cursor])
	    (set! click-regions null)
	    (set! interactives #hash())
	    (set! adjust-cursor void)
	    (paint-slide canvas prefetch-dc n)
	    (set! prefetched-click-regions click-regions)
	    (set! prefetched-interactives interactives)
	    (set! click-regions old-click-regions)
	    (set! interactives old-interactives)
            (set! adjust-cursor old-adjust))
	  (set! prefetched-page n)
	  (when (and config:use-prefetch-in-preview?
		     (send f-both is-shown?))
	    (send c-both paint-prefetched))))

      (define (schedule-slide-prefetch canvas n delay-msec)
	(cancel-prefetch)
	(when (and config:use-prefetch?
		   (not (equal? n prefetched-page)))
	  (let ([b (box #t)])
	    (set! prefetch-schedule-cancel-box b)
	    (new timer% [interval delay-msec] [just-once? #t]
		 [notify-callback (lambda ()
				    (when (unbox b)
				      (if (pair? current-transitions)
					  ;; try again to wait for transition to end
					  (schedule-slide-prefetch canvas n delay-msec)
					  ;; Build next slide...
					  (prefetch-slide canvas n))))]))))


      (define (cancel-prefetch)
	(set-box! prefetch-schedule-cancel-box #f))

      (define c (make-object c% f))
      (define c-both (make-object two-c% f-both))

      (define (viewer:pict->pre-render-pict p)
        (cond
          [(and (not config:printing?)
                (eq? (system-type) 'macosx))
           (let ([bm (send c make-bitmap
                           (inexact->exact (ceiling (pict-width p)))
                           (inexact->exact (ceiling (pict-height p))))])
             (define dc (send bm make-dc))
             (when config:smoothing?
               (send dc set-smoothing 'aligned))
             ((make-pict-drawer p) dc 0 0)
             (send dc set-bitmap #f)
             (refocus (lt-superimpose (ghost p) (bitmap bm)) p))]
          [else (refocus (cc-superimpose (blank) p) p)]))

      (define time-update-thread #f)
      (define (start-time-update!)
        (when config:show-time?
          (unless time-update-thread
            (set! time-update-thread
                  (thread (lambda ()
                            (let loop ([prev-minute #f])
                              (define m (date-minute (seconds->date (current-seconds))))
                              (unless (equal? m prev-minute)
                                (queue-callback
                                 (lambda () (send c refresh))))
                              (sleep 1)
                              (loop m))))))))
      (define (stop-time-update!)
        (when time-update-thread
          (kill-thread time-update-thread)
          (set! time-update-thread #f))
        (stop-elapsed-timer!))

      (define refresh-page
	(lambda ([immediate-prefetch? #f])
	  (hide-cursor-until-moved)
          (set! spotlight-shown? #f)
	  (send f set-blank-cursor #t)
	  (when (= current-page 0)
	    (set! start-time #f)
	    (unless start-time
	      (set! start-time (current-seconds))))
	  (send c redraw)
	  (when (and c-both (send f-both is-shown?))
	    (send c-both redraw))
	  (when (< current-page (- slide-count 1))
	    (schedule-slide-prefetch c
                                     (+ current-page 1)
				     (if immediate-prefetch?
					 50
					 500)))))

      (define current-transitions null)
      (define current-transitions-key #f)
      (define current-timeout-key #f)

      (define (do-transitions transes offscreen)
	(let ([key (cons 1 2)])
	  (set! current-transitions (map (lambda (mk) (mk offscreen)) transes))
	  (set! current-transitions-key key)
	  (if (null? transes)
	      (refresh-page #t)
	      (let do-trans ()
		(when (and (eq? current-transitions-key key)
			   (pair? current-transitions))
		  (let ([went ((car current-transitions) c offscreen)])
		    (if (eq? went 'done)
			(begin
			  (set! current-transitions (cdr current-transitions))
			  (if (null? current-transitions)
			      (refresh-page #t)
			      (do-trans)))
			(new timer%
			     [just-once? #t]
			     [interval (inexact->exact (floor (* 1000 went)))]
			     [notify-callback (lambda ()
						;; Going through queue-callback
						;;  avoids blocking events
						(queue-callback
						 do-trans
						 #f))]))))))))

      (define (stop-transition)
	(cancel-prefetch)
	(unless (null? current-transitions)
	  (stop-transition/no-refresh)
	  (refresh-page)))

      (define (stop-transition/no-refresh)
	(set! current-transitions null)
	(set! current-transitions-key #f)
        (set! current-timeout-key #f))

      (define (get-page-from-user)
	(unless (zero? slide-count)
	  (letrec ([d (make-object dialog% "Goto Page" f 200 250)]
		   [short-slide-list
		    (let loop ([slides talk-slide-list][n 1][last-title #f])
		      (cond
		       [(null? slides) null]
		       [(and last-title
			     (equal? last-title (or (sliderec-title (car slides))
						    "(untitled)")))
			(loop (cdr slides) (+ n 1) last-title)]
		       [else
			(let ([title (or (sliderec-title (car slides))
					 "(untitled)")])
			  (cons (cons
				 n
				 (format "~a. ~a"
					 (slide-page-string (car slides))
					 title))
				(loop (cdr slides) (add1 n) title)))]))]
		   [long-slide-list (let loop ([slides talk-slide-list][n 1])
				      (if (null? slides)
					  null
					  (cons (cons
						 n
						 (format "~a. ~a"
							 (slide-page-string (car slides))
							 (or (sliderec-title (car slides))
							     "(untitled)")))
						(loop (cdr slides) (add1 n)))))]
		   [slide-list short-slide-list]
		   [l (make-object list-box% #f (map cdr slide-list)
				   d (lambda (l e)
				       (when (eq? (send e get-event-type) 'list-box-dclick)
					 (ok-action))))]
		   [p (make-object horizontal-pane% d)]
		   [ok-action (lambda ()
				(send d show #f)
				(let ([i (send l get-selection)])
				  (when i
				    (set! current-page (sub1 (car (list-ref slide-list i))))
				    (refresh-page))))])
	    (send d center)
	    (send p stretchable-height #f)
	    (make-object check-box% "&All Pages" p
			 (lambda (c e)
			   (set! slide-list (if (send c get-value)
						long-slide-list
						short-slide-list))
			   (send l set (map cdr slide-list))))
	    (make-object pane% p)
	    (make-object button% "Cancel" p (lambda (b e) (send d show #f)))
	    (make-object button% "Ok" p (lambda (b e) (ok-action)) '(border))
	    (send l focus)
	    (send d reflow-container)
	    (let ([now (let loop ([l slide-list][n 0])
			 (if (null? l)
			     (sub1 n)
			     (if (> (sub1 (caar l)) current-page)
				 (sub1 n)
				 (loop (cdr l) (add1 n)))))])
	      (send l set-selection (max 0 now))
	      (send l set-first-visible-item (max 0 (- now 3))))
	    (send d show #t))))

      (send f reflow-container)
      (send f-both reflow-container)

      (refresh-page)

      (define slideshow-bm
	(include-bitmap (lib "slideshow/slideshow.png")))
      (define slideshow-mbm
	(include-bitmap (lib "slideshow/mask.xbm")))

      (let* ([bm slideshow-bm]
	     [mbm slideshow-mbm])
	(when (send bm ok?)
	  (send f set-icon bm (and (send mbm ok?) mbm) 'both)))

      (when (and config:commentary?
		 (not config:commentary-on-slide?))
	(send c-frame show #t)
	(message-box "Instructions"
		     (format "Keybindings:~
                     \n  {Meta,Alt}-q - quit~
                     \n  Right, Space, f or n - next slide~
                     \n  Left, b - prev slide~
                     \n  g - last slide~
                     \n  1 - first slide~
                     \n  {Meta,Alt}-g - select slide~
                     \n  p - show/hide slide number~
                     \n  {Meta,Alt}-c - show/hide commentary~
                     \n  {Meta,Alt,Shift}-{Right,Left,Up,Down} - move window~
                     \nAll bindings work in all windows")))

      (define (do-print)
	(let ([ps-dc (dc-for-text-size)])
          (maybe-set-smoothing ps-dc) ; for printer-dc%
	  (let loop ([start? #f][l (list-tail talk-slide-list current-page)][n current-page])
	    (unless (null? l)
	      (set! current-page n)
	      (refresh-page)
              (define aspect (sliderec-aspect (car l)))
	      (when start?
		(send ps-dc start-page))
	      (let ([slide (car l)])
		(let ([xs (/ (hash-ref current-use-screen-ws aspect)
                             (hash-ref config:screen-ws aspect))]
		      [ys (/ (hash-ref current-use-screen-hs aspect)
                             (hash-ref config:screen-hs aspect))])
		  (send ps-dc set-scale xs ys)
                  (let ([clip (send ps-dc get-clipping-region)]
                        [dx (/ (- config:actual-screen-w
                                  (hash-ref current-use-screen-ws aspect))
                               2 xs)]
                        [dy (/ (- config:actual-screen-h
                                  (hash-ref current-use-screen-hs aspect))
                               2 ys)])
                    (send ps-dc set-clipping-rect dx dy (hash-ref config:screen-ws aspect) (hash-ref config:screen-hs aspect))
                    ((sliderec-drawer slide) ps-dc (+ margin dx) (+ margin dy))
                    (send ps-dc set-clipping-region clip)))
		(when show-page-numbers?
		  (send ps-dc set-scale 1 1)
		  (let ([s (slide-page-string slide)])
		    (let-values ([(w h) (send ps-dc get-size)]
				 [(sw sh sd sa) (send ps-dc get-text-extent s)]
				 [(hm vm) (values margin margin)])
		      (send ps-dc draw-text s (- w hm sw) (- h vm sh))))))
	      (send ps-dc end-page)
	      (loop #t (cdr l) (add1 n))))
	  (parameterize ([current-security-guard original-security-guard])
	    (send ps-dc end-doc))))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                Progress for Print             ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define report-progress
        (case config:progress-mode
          [(none) void]

          [(text)
           (lambda ()
             (printf "\rBuilding slide ~a..." slide-count)
             (flush-output))]

          [(gui)
           (parameterize ([current-eventspace (make-eventspace)])
             (define frame (make-object (class frame%
                                          (define/augment (on-close) (exit))
                                          (super-instantiate ()))
                             "Progress"))
             (define panel (instantiate horizontal-panel% (frame)
                             (stretchable-width #f)
                             (stretchable-height #f)))
             (make-object message% "Building slide: " panel)

             (define display (make-object message% "0000" panel))
             (send display set-label "1")
             (send frame center)
             (send frame show #t)
             
             (lambda ()
               (send display set-label (number->string slide-count))))]))

      (define (viewer:done-making-slides)
	(when config:printing?
          (when (eq? config:progress-mode 'text)
            (displayln " done.")
            (display "Printing slides...")
            (flush-output))

          (do-print)

          (when (eq? config:progress-mode 'text)
            (displayln " done."))

          (exit)))

      (when config:printing?
        ;; Just before exiting normally, print the slides:
        (let ([h (executable-yield-handler)])
          (executable-yield-handler
           (lambda (v)
             (viewer:done-making-slides)
             (h v)))))

      (let ([eh (uncaught-exception-handler)])
	(uncaught-exception-handler
	 (lambda (exn)
	   (send f show #f)
	   (when f-both
	     (send f-both show #f))
	   (when background-f
	     (send background-f show #f))
          (stop-time-update!)
          (stop-elapsed-timer!)
          (eh exn))))))
