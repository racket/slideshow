
(module viewer mzscheme
  (require (lib "class.ss")
           (lib "unit.ss")
           (lib "unitsig.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "contract.ss")
	   (lib "mred.ss" "mred")
	   (lib "mrpict.ss" "texpict")
	   (lib "utils.ss" "texpict")
	   (lib "math.ss")
	   (lib "list.ss")
	   "sig.ss"
	   "core.ss"
	   "util.ss")

  (provide viewer@)

  (define viewer@
    (unit/sig viewer^
      (import (config : cmdline^) core^)
      (rename (viewer:set-use-background-frame! set-use-background-frame!)
	      (viewer:enable-click-advance! enable-click-advance!)
	      (viewer:set-page-numbers-visible! set-page-numbers-visible!))

      (define-accessor margin get-margin)
      (define-accessor client-w get-client-w)
      (define-accessor client-h get-client-h)

      (define current-page 0)
      (define use-background-frame? #f)
      (define show-page-numbers? #t)
      (define click-to-advance? #t)
      (define click-regions null)
      (define talk-slide-list null)
      (define talk-slide-reverse-cell-list null)

      (define empty-slide (make-sliderec (make-pict-drawer
					  (let ([link (lambda (label thunk)
							(clickback (colorize 
								    (let ([p (t label)])
								      (refocus (vc-append p
											  (linewidth 2 (hline (pict-width p) 2)))
									       p))
								    "blue")
								   thunk))])
					    (cc-superimpose
					     (get-titleless-page)
					     (vl-append
					      gap-size
					      (bt "Slideshow")
					      (page-item "If you supplied a talk, it is loading...")
					      (page-item "Or " 
							 (link "run tutorial"
							       (lambda ()
								 (load-content
								  (build-path (collection-path "slideshow")
									      "tutorial-show.ss")))))
					      (page-item "Or " 
							 (link "choose talk"
							       (lambda ()
								 (let ([file (get-file)])
								   (when file
								     (let-values ([(base name dir?) (split-path file)])
								       (current-directory base))
								     (send f show #f)
								     (load-content file))))))))))
					 "<Empty>"
					 #f
					 0
					 1
					 zero-inset
					 null))

      (define (talk-list-ref n)
	(if (null? talk-slide-list)
	    empty-slide
	    (list-ref talk-slide-list n)))

      (define (add-talk-slide! s)
	(let ([p (cons s null)])
	  (if (null? talk-slide-reverse-cell-list)
	      (set! talk-slide-list p)
	      (set-cdr! (car talk-slide-reverse-cell-list) p))
	  (set! talk-slide-reverse-cell-list (cons p talk-slide-reverse-cell-list)))
	(send f slide-changed (sub1 (length talk-slide-list)))
	(yield))
      (define (retract-talk-slide!)
	(unless (null? talk-slide-reverse-cell-list)
	  (set! talk-slide-reverse-cell-list (cdr talk-slide-reverse-cell-list))
	  (if (null? talk-slide-reverse-cell-list)
	      (set! talk-slide-list null)
	      (set-cdr! (car talk-slide-reverse-cell-list) null)))
	(if config:printing?
	    (send progress-display set-label (number->string (length talk-slide-list)))
	    (begin
	      (send f slide-changed (length talk-slide-list))
	      (yield))))
      (define (most-recent-talk-slide)
	(and (pair? talk-slide-reverse-cell-list)
	     (caar talk-slide-reverse-cell-list)))
      
      (define (set-init-page! p)
	(set! current-page p))
      (set-init-page! config:init-page)
      
      (define (viewer:set-use-background-frame! on?)
	(set! use-background-frame? (and on? #t)))
      
      (define (viewer:enable-click-advance! on?)
	(set! click-to-advance? (and on? #t)))
      
      (define (viewer:set-page-numbers-visible! on?)
	(set! show-page-numbers? (and on? #t)))
      (viewer:set-page-numbers-visible! config:show-page-numbers?)
      
      (define (add-click-region! cr)
	(set! click-regions (cons cr click-regions)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                   Main GUI                    ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      #;
      (when config:quad-view?
	(set! talk-slide-list
	      (let loop ([l talk-slide-list])
		(cond
		 [(null? l) null]
		 [(< (length l) 4)
		  (loop (append l (vector->list
				   (make-vector
				    (- 4 (length l))
				    (make-sliderec void #f #f 
						   (sliderec-page (car (last-pair l)))
						   1 
						   zero-inset 
						   null)))))]
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
				  (send dc set-scale (* orig-sx scale) (* orig-sy scale))
				  (set-origin x y)
				  ((sliderec-drawer a) dc 0 0)
				  (set-origin (+ x (/ client-w 2) margin) y)
				  ((sliderec-drawer b) dc 0 0)
				  (set-origin x (+ y (/ client-h 2) margin))
				  ((sliderec-drawer c) dc 0 0)
				  (set-origin (+ x (/ client-w 2) margin) (+ y (/ client-h 2) margin))
				  ((sliderec-drawer d) dc 0 0)
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
				null)
			       (loop (cddddr l))))]))))
	  
      (define GAUGE-WIDTH 100)
      (define GAUGE-HEIGHT 4)

      (define talk-start-seconds (current-seconds))
      (define slide-start-seconds (current-seconds))
      
      (define talk-frame%
	(class frame% 
	  (init-field closeable?)
	  (init-field close-bg?)
	  (define/augment can-close? (lambda () (and closeable? (inner #t can-close?))))
	  (define/override on-superwindow-show (lambda (on?)
						 (unless on?
						   (when (and close-bg? background-f)
						     (send background-f show #f)))))

	  (define/override on-subwindow-char
	    (lambda (w e)
	      (let ([k (send e get-key-code)])
		(case k
		  [(right)
		   (shift e 1 0 (lambda () (next)))]
		  [(left)
		   (shift e -1 0 (lambda () (prev)))]
		  [(up)
		   (shift e 0 -1 void)]
		  [(down)
		   (shift e 0 1 void)]
		  [(#\space #\f #\n)
		   (next)
		   #t]
		  [(#\b #\backspace #\rubout)
		   (prev)
		   #t]
		  [(#\g)
		   (stop-transition)
		   (if (send e get-meta-down)
		       (get-page-from-user)
		       (begin
			 (set! current-page (max 0 (sub1 (length talk-slide-list))))
			 (refresh-page)))
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
		   (when (equal? 1 (message-box/custom
				    "Quit"
				    "Really quit the slide show?"
				    "&Quit"
				    "&Cancel"
				    #f
				    this
				    '(default=1 caution)))
		     (stop-show))
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
		  [else
		   #f]))))

	  (define/private (stop-show)
	    (send c-frame show #f)
	    (send f show #f)
	    (send f-both show #f)
	    (when config:print-slide-seconds?
	      (printf "Total Time: ~a seconds~n"
		      (- (current-seconds) talk-start-seconds))))
	  
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
	      (move (+ x dx) (+ y dy))))
	  
	  (define/private (prev)
	    (stop-transition)
	    (set! current-page (max (sub1 current-page)
				    0))
	    (refresh-page))
	  
	  (define/public (next)
	    (if (pair? current-transitions)
		(stop-transition)
		(change-slide 1)))

	  (define/public (slide-changed pos)
	    (when (or (= pos current-page)
		      (and (or config:use-prefetch?
			       (send f-both is-shown?))
			   (= pos (add1 current-page))))
	      (stop-transition)
	      (change-slide 0)))

	  (define/private (change-slide n)
	    (let ([old (talk-list-ref current-page)])
	      (set! current-page (max 0
				      (min (+ n current-page)
					   (sub1 (length talk-slide-list)))))
	      (when config:print-slide-seconds?
		(let ([slide-end-seconds (current-seconds)])
		  (printf "Slide ~a: ~a seconds~n" current-page
			  (- slide-end-seconds slide-start-seconds))
		  (set! slide-start-seconds slide-end-seconds)))
	      ;; Refresh screen, and start transitions from old, if any
	      (do-transitions (if config:use-transitions?
				  (sliderec-transitions old)
				  null)
			      (send c get-offscreen))))

	  (super-new)))
      
      (define-values (screen-left-inset screen-top-inset)
	(if config:keep-titlebar?
	    (values 0 0)
	    (get-display-left-top-inset)))

      (define background-f
	(and use-background-frame?
	     (make-object (class frame%
			    (define/override (on-activate on?)
			      (when on?
				(send f show #t)))
			    (super-instantiate 
			     ()
			     [label "Slidsehow Background"]
			     [x (- screen-left-inset)] [y (- screen-top-inset)]
			     [width (inexact->exact (floor config:actual-screen-w))]
			     [height (inexact->exact (floor config:actual-screen-h))]
			     [style '(no-caption no-resize-border hide-menu-bar)])))))

      (when background-f
	(send background-f enable #f)
	(send background-f show #t))

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
				'(no-caption no-resize-border hide-menu-bar))]))
      
      (define f-both (new talk-frame%
			  [closeable? #t]
			  [close-bg? #f]
			  [label "Slideshow Preview"]
			  [x 0] [y 0]
			  [width (inexact->exact (floor config:actual-screen-w))]
			  [height (inexact->exact (quotient (floor config:actual-screen-h) 2))]
			  [style '()]))
      
      (define current-sinset zero-inset)
      (define resizing-frame? #f)
      (define (reset-display-inset! sinset)
	(unless (and (= (sinset-l current-sinset) (sinset-l sinset))
		     (= (sinset-t current-sinset) (sinset-t sinset))
		     (= (sinset-r current-sinset) (sinset-r sinset))
		     (= (sinset-b current-sinset) (sinset-b sinset)))
	  (set! resizing-frame? #t) ; hack --- see yield below
	  (send f resize 
		(max 1 (- (inexact->exact (floor config:actual-screen-w)) 
			  (inexact->exact (floor (* (+ (sinset-l sinset) (sinset-r sinset))
						    (/ config:actual-screen-w config:screen-w))))))
		(max 1 (- (inexact->exact (floor config:actual-screen-h)) 
			  (inexact->exact (floor (* (+ (sinset-t sinset) (sinset-b sinset))
						    (/ config:actual-screen-h config:screen-h)))))))
	  (send f move 
		(inexact->exact (- (floor (* (sinset-l sinset) 
					     (/ config:actual-screen-w config:screen-w))) 
				   screen-left-inset))
		(inexact->exact (- (floor (* (sinset-t sinset) 
					     (/ config:actual-screen-h config:screen-h))) 
				   screen-top-inset)))
	  (set! current-sinset sinset)
	  ;; FIXME: This yield is here so that the frame
	  ;;  and its children can learn about their new
	  ;;  sizes, and so that the generated on-size callback
	  ;;  can be ignored. Obviously, using yield creates a
	  ;;  kind of race condition for incoming events from the user.
	  (yield)
	  (set! resizing-frame? #f)))
      

      (define c-frame (new talk-frame%
			   [closeable? #t]
			   [close-bg? #f]
			   [label "Commentary"]
			   [width 400]
			   [height 100]))
      (define commentary (make-object text%))
      (send (make-object editor-canvas% c-frame commentary)
	    set-line-count 3)
      (send commentary auto-wrap #t)
      (send c-frame reflow-container)
      
      (define start-time #f)
      
      (define clear-brush (make-object brush% "WHITE" 'transparent))
      (define white-brush (make-object brush% "WHITE" 'solid))
      (define gray-brush (make-object brush% "GRAY" 'solid))
      (define green-brush (make-object brush% "GREEN" 'solid))
      (define red-brush (make-object brush% "RED" 'solid))
      (define black-brush (make-object brush% "BLACK" 'solid))
      (define black-pen (make-object pen% "BLACK" 1 'solid))
      (define clear-pen (make-object pen% "BLACK" 1 'transparent))
      (define red-color (make-object color% "RED"))
      (define green-color (make-object color% "GREEN"))
      (define black-color (make-object color% "BLACK"))

      (define (slide-page-string slide)
	(if (= 1 (sliderec-page-count slide))
	    (format "~a" (sliderec-page slide))
	    (format "~a-~a" (sliderec-page slide) (+ (sliderec-page slide)
						     (sliderec-page-count slide)
						     -1))))
      
      (define (calc-progress)
	(if (and start-time config:talk-duration-minutes)
	    (values (min 1 (/ (- (current-seconds) start-time) (* 60 config:talk-duration-minutes)))
		    (/ current-page (max 1 (sub1 (length talk-slide-list)))))
	    (values 0 0)))
      
      (define (show-time dc w h)
	(let* ([left (- w GAUGE-WIDTH)]
	       [top (- h GAUGE-HEIGHT)]
	       [b (send dc get-brush)]
	       [p (send dc get-pen)])
	  (send dc set-pen black-pen)
	  (send dc set-brush (if start-time gray-brush clear-brush))
	  (send dc draw-rectangle left top GAUGE-WIDTH GAUGE-HEIGHT)
	  (when start-time
	    (let-values ([(duration distance) (calc-progress)])
	      (send dc set-brush (if (< distance duration)
				     red-brush
				     green-brush))
	      (send dc draw-rectangle left top (floor (* GAUGE-WIDTH distance)) GAUGE-HEIGHT)
	      (send dc set-brush clear-brush)
	      (send dc draw-rectangle left top (floor (* GAUGE-WIDTH duration)) GAUGE-HEIGHT)))
	  (send dc set-pen p)
	  (send dc set-brush b)))
      
      (define c%
	(class canvas%
	  (inherit get-dc get-client-size)
	  
	  (define clicking #f)
	  (define clicking-hit? #f)
	  
	  (define/override (on-paint)
	    (let ([dc (get-dc)])
	      (stop-transition/no-refresh)
	      (cond
	       [config:use-offscreen?
		(let ([bm (send offscreen get-bitmap)])
		  (send (get-dc) draw-bitmap bm 0 0))]
	       [else
		(send dc clear)
		(paint-slide dc)])))
	  
	  (inherit get-top-level-window)
	  (define/override (on-event e)
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
			(invert-clicking!))
		      ((click-region-thunk c)))))]
	     [(and clicking (send e dragging?))
	      (let ([hit? (click-hits? e clicking)])
		(unless (eq? hit? clicking-hit?)
		  (set! clicking-hit? hit?)
		  (invert-clicking!)))]
	     [(and clicking (send e button-up?))
	      (let ([hit? (click-hits? e clicking)]
		    [c clicking])
		(unless (eq? hit? clicking-hit?)
		  (set! clicking-hit? hit?)
		  (invert-clicking!))
		(when clicking-hit?
		  (invert-clicking!))
		(set! clicking #f)
		(when hit?
		  ((click-region-thunk c))))]
	     [(send e button-up?)
	      (when click-to-advance?
		(send (get-top-level-window) next))]
	     [else 
	      (when (and clicking clicking-hit?)
		(invert-clicking!))
	      (set! clicking #f)]))

	  
	  (define/private (click-hits? e c)
	    (let ([x (send e get-x)]
		  [y (send e get-y)])
	      (and (<= (click-region-left c) x (click-region-right c))
		   (<= (click-region-top c) y (click-region-bottom c)))))
	  (define/private (invert-clicking!)
	    (let* ([dc (get-dc)]
		   [b (send dc get-brush)]
		   [p (send dc get-pen)])
	      (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
	      (send dc set-brush  (send the-brush-list find-or-create-brush "black" 'xor))
	      (send dc draw-rectangle 
		    (click-region-left clicking)
		    (click-region-top clicking)
		    (- (click-region-right clicking) (click-region-left clicking))
		    (- (click-region-bottom clicking) (click-region-top clicking)))
	      (send dc set-pen p)
	      (send dc set-brush b)))
	  
	  (define offscreen #f)
	  (define/public get-offscreen (lambda () offscreen))
	  
	  (define/private (shift-click-region cr dx dy)
	    (make-click-region (+ dx (click-region-left cr))
			       (+ dy (click-region-top cr))
			       (+ dx (click-region-right cr))
			       (+ dy (click-region-bottom cr))
			       (click-region-thunk cr)
			       (click-region-show-click? cr)))

	  (define/private (paint-prefetch dc)
	    (let-values ([(cw ch) (get-client-size)])
	      (paint-letterbox dc cw ch config:use-screen-w config:use-screen-h)
	      (let ([dx (floor (/ (- cw config:use-screen-w) 2))]
		    [dy (floor (/ (- ch config:use-screen-h) 2))])
		(send dc draw-bitmap prefetch-bitmap dx dy)
		(set! click-regions (map (lambda (cr)
					   (shift-click-region cr dx dy))
					 prefetched-click-regions)))))

	  (define/override (on-size w h)
	    (unless resizing-frame?
	      (redraw)))

	  (define/public (redraw)
	    (reset-display-inset! (sliderec-inset (talk-list-ref current-page)))
	    (send commentary lock #f)
	    (send commentary erase)
	    (let ([s (talk-list-ref current-page)])
	      (when (just-a-comment? (sliderec-comment s))
		(send commentary insert (just-a-comment-text (sliderec-comment s)))))
	    (send commentary lock #t)
	    (set! click-regions null)
	    (set! clicking #f)
	    (stop-transition/no-refresh)
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
		(paint-slide offscreen)])
	      (let ([bm (send offscreen get-bitmap)])
		(send (get-dc) draw-bitmap bm 0 0))]
	     [(equal? prefetched-page current-page)
	      (paint-prefetch (get-dc))]
	     [else
	      (let ([dc (get-dc)])
		(send dc clear)
		(paint-slide dc))]))
	  (super-new)))

      (define two-c%
	(class canvas%
	  (inherit get-dc)

	  (define/public (paint-prefetched)
	    (let ([dc (get-dc)])
	      (let*-values ([(cw ch) (send dc get-size)])
		(send dc set-scale 
		      (/ (/ cw 2) (send prefetch-bitmap get-width))
		      (/ ch (send prefetch-bitmap get-height)))
		(send dc set-origin (/ cw 2) 0)
		(send dc draw-bitmap prefetch-bitmap 0 0)
		(send dc set-origin 0 0)
		(send dc set-scale 1 1)
		(send dc draw-line (/ cw 2) 0 (/ cw 2) ch))))

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
		      (send dc set-origin (/ cw 2) 0)
		      (send dc draw-bitmap prefetch-bitmap 0 0)]
		     [else
		      (when (< (add1 current-page) (length talk-slide-list))
			(let ([b (send dc get-brush)])
			  (send dc set-brush gray-brush)
			  (send dc draw-rectangle bw 0 bw bh)
			  (send dc set-brush b)))])
		    (send dc set-scale 1 1))]
		 [else
		  (paint-slide dc current-page 1/2 1/2 cw (* 2 ch) cw (* 2 ch) #f)
		  (send dc set-origin (/ cw 2) 0)
		  (when (< (add1 current-page) (length talk-slide-list))
		    (paint-slide dc
				 (+ current-page 1)
				 1/2 1/2
				 cw (* 2 ch) cw (* 2 ch)
				 #f))])
		(send dc set-origin 0 0)
		(send dc draw-line (/ cw 2) 0 (/ cw 2) ch))))
	  
	  (inherit get-top-level-window)
	  (define/override (on-event e)
	    (cond
	     [(send e button-up?)
	      (send (get-top-level-window) next)]))
	  
	  (define/public (redraw) (on-paint))
	  (super-new)))

      (define (paint-letterbox dc cw ch usw ush)
	(when (or (< usw cw)
		  (< ush ch))
	  (let ([b (send dc get-brush)]
		[p (send dc get-pen)])
	    (send dc set-brush black-brush)
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
	    (send dc set-pen p))))

      (define paint-slide
	(case-lambda
	 [(dc) (paint-slide dc current-page)]
	 [(dc page) 
	  (let-values ([(cw ch) (send dc get-size)])
	    (paint-slide dc page 1 1 cw ch config:use-screen-w config:use-screen-h #t))]
	 [(dc page extra-scale-x extra-scale-y cw ch usw ush to-main?)
	  (let* ([slide (talk-list-ref page)]
		 [ins (sliderec-inset slide)]
		 [cw (if to-main?
			 (+ cw (sinset-l ins) (sinset-r ins))
			 cw)]
		 [ch (if to-main?
			 (+ ch (sinset-t ins) (sinset-b ins))
			 ch)]
		 [sx (/ usw config:screen-w)]
		 [sy (/ ush config:screen-h)]
		 [mx (/ (- cw usw) 2)]
		 [my (/ (- ch ush) 2)])
	    (paint-letterbox dc cw ch usw ush)
	    
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
		(send dc set-font f))))]))

      ;; prefetched-page : (union #f number)
      (define prefetched-page #f)
      ;; prefetch-bitmap : (union #f bitmap)
      (define prefetch-bitmap #f)
      ;; prefetch-bitmap : (union #f bitmap-dc)
      (define prefetch-dc #f)
      ;; prefetch-schedule-cancel-box : (box boolean)
      (define prefetch-schedule-cancel-box (box #f))
      ;; prefetched-click-regions : list
      (define prefetched-click-regions null)

      (define (prefetch-slide n)
	(set! prefetched-page #f)
	
	(unless prefetch-dc
	  (set! prefetch-dc (new bitmap-dc%)))

	;; try to re-use existing bitmap
	(unless (and (is-a? prefetch-bitmap bitmap%)
		     (= config:use-screen-w (send prefetch-bitmap get-width))
		     (= config:use-screen-h (send prefetch-bitmap get-height)))
	  (send prefetch-dc set-bitmap #f)
	  (set! prefetch-bitmap (make-bitmap config:use-screen-w config:use-screen-h))
	  (when (send prefetch-bitmap ok?)
	    (send prefetch-dc set-bitmap prefetch-bitmap)))

	(when (send prefetch-dc ok?)
	  (send prefetch-dc clear)
	  (let ([old-click-regions click-regions])
	    (set! click-regions null)
	    (paint-slide prefetch-dc n)
	    (set! prefetched-click-regions click-regions)
	    (set! click-regions old-click-regions))
	  (set! prefetched-page n)
	  (when (and config:use-prefetch-in-preview?
		     (send f-both is-shown?))
	    (send c-both paint-prefetched))))

      (define (schedule-slide-prefetch n delay-msec)
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
					  (schedule-slide-prefetch n delay-msec)
					  ;; Build next slide...
					  (prefetch-slide n))))]))))


      (define (cancel-prefetch)
	(set-box! prefetch-schedule-cancel-box #f))

      (define c (make-object c% f))
      (define c-both (make-object two-c% f-both))
      
      (define refresh-page
	(opt-lambda ([immediate-prefetch? #f])
	  (when (= current-page 0)
	    (set! start-time #f)
	    (unless start-time
	      (set! start-time (current-seconds))))
	  (send c redraw)
	  (when (and c-both (send f-both is-shown?))
	    (send c-both redraw))
	  (when (< current-page (- (length talk-slide-list) 1))
	    (schedule-slide-prefetch (+ current-page 1)
				     (if immediate-prefetch?
					 50
					 500)))))

      (define current-transitions null)
      (define current-transitions-key #f)

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
	(set! current-transitions-key #f))
      
      (define (get-page-from-user)
	(let* ([d (make-object dialog% "Goto Page" f 200 250)]
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
			       d void)]
	       [p (make-object horizontal-pane% d)])
	  (send d center)
	  (send p stretchable-height #f)
	  (make-object check-box% "All Pages" p
		       (lambda (c e)
			 (set! slide-list (if (send c get-value)
					      long-slide-list
					      short-slide-list))
			 (send l set (map cdr slide-list))))
	  (make-object pane% p)
	  (make-object button% "Cancel" p (lambda (b e) (send d show #f)))
	  (make-object button% "Ok" p 
		       (lambda (b e)
			 (send d show #f)
			 (let ([i (send l get-selection)])
			   (when i
			     (set! current-page (sub1 (car (list-ref slide-list i))))
			     (refresh-page))))
		       '(border))
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
	  (send d show #t)))
      
      (send f reflow-container)
      (send f-both reflow-container)

      (refresh-page)
      
      (let* ([bm (make-object bitmap% (build-path (collection-path "slideshow") "slideshow.png"))]
	     [mbm (make-object bitmap% (build-path (collection-path "slideshow") "mask.xbm"))])
	(when (send bm ok?)
	  (send f set-icon bm (and (send mbm ok?) mbm) 'both)))
      
      (send f show (not config:printing?))
      (when config:two-frames?
	(send f-both show #t))
      
      (when config:commentary?
	(send c-frame show #t)
	(message-box "Instructions"
		     (format "Keybindings:~
                     ~n  {Meta,Alt}-q - quit~
                     ~n  Right, Space, f or n - next slide~
                     ~n  Left, b - prev slide~
                     ~n  g - last slide~
                     ~n  1 - first slide~
                     ~n  {Meta,Alt}-g - select slide~
                     ~n  p - show/hide slide number~
                     ~n  {Meta,Alt}-c - show/hide commentary~
                     ~n  {Meta,Alt,Shift}-{Right,Left,Up,Down} - move window~
                     ~nAll bindings work in all windows")))
      
      (define (do-print)
	(let ([ps-dc (dc-for-text-size)])
	  (let loop ([start? #f][l (list-tail talk-slide-list current-page)][n current-page])
	    (unless (null? l)
	      (set! current-page n)
	      (refresh-page)
	      (when start?
		(send ps-dc start-page))
	      (let ([slide (car l)])
		(let ([xs (/ config:use-screen-w config:screen-w)]
		      [ys (/ config:use-screen-h config:screen-h)])
		  (send ps-dc set-scale xs ys)
		  ((sliderec-drawer slide) ps-dc 
		   (+ margin (/ (- config:actual-screen-w config:use-screen-w) 2 xs))
		   (+ margin (/ (- config:actual-screen-h config:use-screen-h) 2 ys))))
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
	    (send ps-dc end-doc))
	  (exit)))


      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                Progress for Print             ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

      (define-values (progress-window progress-display)
	(if config:printing?
	    (parameterize ([current-eventspace (make-eventspace)])
	      (let* ([f (make-object (class frame% 
				       (define/augment (on-close) (exit))
				       (super-instantiate ()))
				     "Progress")]
		     [h (instantiate horizontal-panel% (f)
				     (stretchable-width #f)
				     (stretchable-height #f))])
		(make-object message% "Building slide: " h)
		(let ([d (make-object message% "0000" h)])
		  (send d set-label "1")
		  (send f center)
		  (send f show #t)
		  (values f d))))
	    (values #f #f)))

      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;                Talk Loading                   ;;
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
      (define original-security-guard (current-security-guard))
      
      (define (load-content content)
	(unless config:trust-me?
	  (current-security-guard
	   (make-security-guard original-security-guard
				(lambda (who what mode)
				  (when (memq 'write mode)
				    (error 'slideshow
					   "slide program attempted to write to filesystem: ~e"
					   what))
				  (when (memq 'execute mode)
				    (error 'slideshow
					   "slide program attempted to execute external code: ~e"
					   what)))
				(lambda (who where-name where-port-num mode)
				  (error 'slideshow
					 "slide program attempted to make a network connection")))))
	(unless config:printing?
	  (send f show #t))
	(dynamic-require (path->complete-path content) #f))

      (when config:file-to-load
	(queue-callback
	 (lambda ()
	   (load-content (string->path config:file-to-load))
	   (when config:printing?
	     (do-print))))))))
