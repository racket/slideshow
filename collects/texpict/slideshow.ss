
(module slideshow mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss"))

  (require (lib "mred.ss" "mred"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                Command Line                   ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (screen-w screen-h) (values 1024 768))

  (define condense? #f)
  (define printing? #f)
  (define commentary? #f)
  (define show-gauge? #f)
  (define show-page-numbers? #t)
  (define quad-view? #f)

  (define base-font-size 28)
  
  (define current-page 0)
  
  (require (lib "cmdline.ss"))

  (define content
    (command-line
     "talk"
     (namespace-variable-binding 'argv)
     [once-each
      (("--print") "print"
		   (set! printing? #t))
      (("-c" "--condense") "condense"
			   (set! condense? #t))
      (("-p") page "set the starting page"
	      (let ([n (string->number page)])
		(unless (and n 
			     (integer? n)
			     (exact? n)
			     (positive? n))
		  (error 'talk "argument to -p is not a positive exact integer: ~a" page))
		(set! current-page (sub1 n))))
      (("-q" "--quad") "show four slides at a time"
		       (set! quad-view? #t))
      (("-f" "--font") fs "set base font size"
		       (let ([n (string->number fs)])
			 (unless (and n 
				      (integer? n)
				      (exact? n)
				      (positive? n))
			   (error 'talk "argument to -f is not a positive exact integer: ~a" fs))
			 (set! base-font-size n)))
      (("-c") "display commentary"
	      (set! commentary? #t))]
     [args (lecture-file)
	   lecture-file]))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                    Setup                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ps-pre-scale 0.8)

  (define font-size base-font-size)
  (define line-sep 2)
  (define title-size (+ font-size 4))
  (define main-font 'swiss)

  (define red "red")
  (define green "forest green")
  (define blue "blue")
  (define purple "purple")
  (define orange "orange")

  (require "mrpict.ss")
  (require "utils.ss")
  (require (lib "math.ss"))

  (define current-font-size (make-parameter 
			     font-size
			     (lambda (x)
			       (unless (and (number? x)
					    (integer? x)
					    (exact? x)
					    (positive? x))
				 (raise-type-error 'current-font-size "exact non-negative integer" x))
			       x)))

  (define (t s) (text s main-font (current-font-size)))
  (define (it s) (text s `(italic . ,main-font) (current-font-size)))
  (define (bt s) (text s `(bold . ,main-font) (current-font-size)))
  (define (bit s) (text s `(bold italic . ,main-font) (current-font-size)))
  (define (tt s) (text s '(bold . modern) (current-font-size)))
  (define (titlet s) (colorize (text s 
				     `(bold . ,main-font) 
				     title-size)
			       green))

  (define bullet (cc-superimpose (disk (/ font-size 2)) 
				 (blank 0 font-size)))
  (define o-bullet (cc-superimpose (circle (/ font-size 2)) 
				   (blank 0 font-size)))

  (dc-for-text-size
   (if printing?
       ;; Make ps-dc%:
       (let ([pss (make-object ps-setup%)])
	 (send pss set-mode 'file)
	 (send pss set-scaling ps-pre-scale ps-pre-scale)
	 (send pss set-orientation 'landscape)
	 (parameterize ([current-ps-setup pss])
	   (let ([p (make-object post-script-dc% #t)])
	     (unless (send p ok?) (exit))
	     (send p start-doc "Slides")
	     (send p start-page)
	     (set!-values (screen-w screen-h) (send p get-size))
	     p)))

       ;; Bitmaps give same size as the screen:
       (make-object bitmap-dc% (make-object bitmap% 1 1))))

  (define margin 20)
  (define-values (client-w client-h) (values (- screen-w (* margin 2))
					     (- screen-h (* margin 2))))
  (define full-page (blank client-w client-h))
  (define titleless-page (inset full-page 0 (- 0 title-size font-size) 0 0))

  (define talk-slide-list null)
  (define-struct slide (drawer title comment page page-count))
  (define-struct comment (text))

  (define page-number 1)

  (define (add-slide! pict title comment page-count)
    (set! talk-slide-list (cons
			   (make-slide (make-pict-drawer pict)
				       title 
				       comment
				       page-number
				       page-count)
			   talk-slide-list))
    (set! page-number (+ page-number page-count))
    (send progress-display set-label (number->string page-number)))

  (define (skip-slides n)
    (set! page-number (+ page-number n)))

  (define (evenize-width p)
    (let ([w (pict-width p)])
      ;; Force even size:
      (inset p 0 0 (+ (- w (floor w)) (modulo (floor w) 2)) 0)))

  (define (one-slide/title process v-sep skipped-pages s . x) 
    (let-values ([(x c)
		  (let loop ([x x][c #f][r null])
		    (cond
		     [(null? x) (values (reverse! r) c)]
		     [(comment? (car x))
		      (loop (cdr x) (car x) r)]
		     [else
		      (loop (cdr x) c (cons (car x) r))]))])
      (add-slide!
       (ct-superimpose
	full-page
	(apply vc-append v-sep
	       (map
		evenize-width
		(if s
		    (cons (titlet s) (process x))
		    (process x)))))
       s
       c
       (+ 1 skipped-pages))))

  (define (do-slide/title/tall process v-sep s . x)
    (let loop ([l x][r null][comment #f][skip-all? #f][skipped 0])
      (cond
       [(null? l) 
	(if skip-all?
	    (add1 skipped)
	    (begin
	      (apply one-slide/title process v-sep skipped s (reverse r))
	      0))]
       [(memq (car l) '(NEXT NEXT!))
	(let ([skip? (or skip-all? (and condense? (eq? (car l) 'NEXT)))])
	  (let ([skipped (if skip?
			     (add1 skipped)
			     (begin
			       (apply one-slide/title process v-sep skipped s (reverse r))
			       0))])
	    (loop (cdr l) r comment skip-all? skipped)))]
       [(memq (car l) '(ALTS ALTS~)) 
	(let ([rest (cddr l)])
	  (let aloop ([al (cadr l)][skipped skipped])
	    (if (null? (cdr al))
		(loop (append (car al) rest) r comment skip-all? skipped)
		(let ([skip? (or skip-all? (and condense? (eq? (car l) 'ALTS~)))])
		  (let ([skipped (loop (car al) r comment skip? skipped)])
		    (aloop (cdr al) skipped))))))]
       [else (loop (cdr l) (cons (car l) r) comment skip-all? skipped)])))

  (define (slide/title/tall s . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title/tall "string" s))
    (apply do-slide/title/tall values font-size s x))

  (define (slide/title s . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title "string" s))
    (apply slide/title/tall s (blank) x))

  (define (slide/title/center s . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title/center "string" s))
    (apply do-slide/title/tall 
	   (lambda (x)
	     (list
	      (cc-superimpose
	       (if s titleless-page full-page)
	       (apply vc-append font-size
		      (map
		       evenize-width
		       x)))))
	   0 s x))

  (define (slide . x) (apply slide/title #f x))

  (define (slide/center . x) (apply slide/title/center #f x))

  (define most-recent-slide
    (case-lambda
     [() (most-recent-slide 0)]
     [(n) (if ((length talk-slide-list) . <= . n)
	      #f
	      (list-ref talk-slide-list n))]))

  (define retract-most-recent-slide
    (lambda ()
      (unless (null? talk-slide-list)
	(let ([slide (car talk-slide-list)])
	  (set! page-number (slide-page slide))
	  (set! talk-slide-list (cdr talk-slide-list))
	  slide))))

  (define (re-slide s)
    (unless (slide? s)
      (raise-type-error 're-slide "slide" s))
    (set! talk-slide-list (cons (make-slide
				 (slide-drawer s)
				 (slide-title s)
				 (slide-comment s)
				 page-number
				 1)
				talk-slide-list))
    (set! page-number (+ page-number 1)))

  (define (make-outline . l)
    (define a (colorize (arrow font-size 0) blue))
    (lambda (which)
      (slide/title
       "Outline"
       (inset
	(lc-superimpose
	 (blank (pict-width full-page) 0)
	 (let loop ([l l])
	   (cond
	    [(null? l) (blank)]
	    [else
	     (vl-append
	      font-size
	      (hbl-append
	       (quotient font-size 2)
	       ((if (eq? which (car l)) values ghost) a)
	       bullet
	       (let ([p (cadr l)])
		 (if (pict? p)
		     p
		     (bt p))))
	      (loop (cdddr l)))])))
	0 font-size 0 0))))

  (define (comment . s) (make-comment
			 (apply string-append s)))

  ;----------------------------------------

  (define (para*/align v-append w . s)
    (define space (t " "))
    (let loop ([pre #f][s s][rest null])
      (cond
       [(null? s)
	(if (null? rest)
	    (or pre (blank))
	    (loop pre (car rest) (cdr rest)))]
       [(list? s) (loop pre (car s) (append (cdr s) rest))]
       [else
	(let* ([sep? (and (string? s) (regexp-match "^[',. :;-]" s))]
	       [p (if (string? s) (t s) s)])
	  (cond
	   [(< (+ (if pre (pict-width pre) 0)
		  (if pre (if sep? 0 (pict-width space)) 0)
		  (pict-width p)) 
	       w)
	    ; small enough
	    (loop (if pre 
		      (hbl-append pre (if sep? (blank) space) p) 
		      p)
		  rest null)]
	   [(and (string? s) (regexp-match "(.*) (.*)" s))
	    ; can break on string
	    => (lambda (m)
		 (loop pre
		       (cadr m) 
		       (cons
			(caddr m)
			rest)))]
	   [(not pre)
	    (v-append
	     line-sep
	     p
	     (loop #f rest null))]
	   [else
	    (v-append
	     line-sep
	     (or pre (blank))
	     (loop p rest null))]))])))

  (define (para* w . s)
    (para*/align vl-append w s))

  (define (para*/r w . s)
    (para*/align vr-append w s))

  (define (para*/c w . s)
    (para*/align vc-append w s))


  (define (para/align v-append w . s)
    (lbl-superimpose (para*/align v-append w s)
		     (blank w 0)))

  (define (para w . s)
    (para/align vl-append w s))

  (define (para/r w . s)
    (para/align vr-append w s))

  (define (para/c w . s)
    (para/align vc-append w s))


  (define (page-para*/align v-append . s)
    (para*/align v-append client-w s))

  (define (page-para* . s)
    (page-para*/align vl-append s))

  (define (page-para*/r . s)
    (page-para*/align vr-append s))

  (define (page-para*/c . s)
    (page-para*/align vc-append s))


  (define (page-para/align v-append . s)
    (para/align v-append client-w s))

  (define (page-para . s)
    (page-para/align vl-append s))

  (define (page-para/r . s)
    (page-para/align vr-append s))

  (define (page-para/c . s)
    (page-para/align vc-append s))

  ;----------------------------------------

  (define (l-combiner para w l)
    (apply
     vl-append
     font-size
     (map (lambda (x) (para w x)) l)))

  ;----------------------------------------

  (define (item* w . s)
    (htl-append (/ font-size 2)
		bullet 
		(para* (- w
			  (pict-width bullet) 
			  (/ font-size 2)) 
		       s)))

  (define (item w . s)
    (lbl-superimpose (item* w s)
		     (blank w 0)))

  (define (page-item* . s)
    (item* client-w s))

  (define (page-item . s)
    (item client-w s))

  ;----------------------------------------

  (define (subitem* w . s)
    (inset (htl-append (/ font-size 2)
		       o-bullet 
		       (para* (- w
				 (* 2 font-size)
				 (pict-width bullet) 
				 (/ font-size 2)) 
			      s))
	   (* 2 font-size) 0 0 0))

  (define (subitem w . s)
    (lbl-superimpose (subitem* w s)
		     (blank w 0)))

  (define (page-subitem* . s)
    (subitem* client-w s))

  (define (page-subitem . s)
    (subitem client-w s))

  ;----------------------------------------

  (define (paras* w . l)
    (l-combiner para* w l))

  (define (paras w . l)
    (l-combiner para w l))

  (define (page-paras* . l)
    (l-combiner (lambda (x y) (page-para* y)) client-w l))

  (define (page-paras . l)
    (l-combiner (lambda (x y) (page-para y)) client-w l))

  ;----------------------------------------

  (define (itemize w . l)
    (l-combiner item w l))

  (define (itemize* w . l)
    (l-combiner item* w l))

  (define (page-itemize . l)
    (l-combiner (lambda (x y) (page-item y)) client-w l))

  (define (page-itemize* . l)
    (l-combiner (lambda (x y) (page-item* y)) client-w l))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                 Talk                          ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (provide (all-from mzscheme)
	   slide slide/title slide/title/tall 
	   slide/center slide/title/center
	   most-recent-slide retract-most-recent-slide re-slide 
	   comment make-outline
	   item item* page-item page-item*
	   subitem subitem* page-subitem page-subitem*
	   itemize itemize* page-itemize page-itemize*
	   para para* page-para page-para*
	   para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
	   font-size current-font-size line-sep title-size main-font
	   red green blue purple orange
	   t it bt bit tt titlet
	   bullet o-bullet
	   margin client-w client-h
	   full-page titleless-page
	   printing? condense? skip-slides
	   (all-from "mrpict.ss")
	   (all-from "utils.ss"))

  (define-values (progress-window progress-display)
    (parameterize ([current-eventspace (make-eventspace)])
      (let* ([f (make-object (class frame% 
			       (override on-close)
			       (define (on-close) (exit))
			       (super-instantiate ()))
			     "Progress")]
	     [h (instantiate horizontal-panel% (f)
			     (stretchable-width #f)
			     (stretchable-height #f))])
	(make-object message% "Building slide: " h)
	(let ([d (make-object message% "0000" h)])
	  (send d set-label "1")
	  (send f center)
	  (values f d)))))

  (send progress-window show #t)

  (dynamic-require `(file ,content) #f)

  (send progress-window show #f)

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                 Talk Viewer                   ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (set! talk-slide-list (reverse talk-slide-list))

  (when quad-view?
    (set! talk-slide-list
	  (let loop ([l talk-slide-list])
	    (cond
	     [(null? l) null]
	     [(< (length l) 4)
	      (loop (append l (vector->list
			       (make-vector
				(- 4 (length l))
				(make-slide void #f #f page-number 1)))))]
	     [else (let ([a (car l)]
			 [b (cadr l)]
			 [c (caddr l)]
			 [d (cadddr l)]
			 [untitled "(untitled)"])
		     (cons (make-slide
			    (lambda (dc x y)
			      (define scale (min (/ (- (/ client-h 2) margin) client-h)
						 (/ (- (/ client-w 2) margin) client-w)))
			      (send dc set-scale scale scale)
			      (send dc set-origin x y)
			      ((slide-drawer a) dc 0 0)
			      (send dc set-origin (+ x (/ client-w 2) margin) y)
			      ((slide-drawer b) dc 0 0)
			      (send dc set-origin x (+ y (/ client-h 2) margin))
			      ((slide-drawer c) dc 0 0)
			      (send dc set-origin (+ x (/ client-w 2) margin) (+ y (/ client-h 2) margin))
			      ((slide-drawer d) dc 0 0)
			      (send dc set-scale 1 1)
			      (send dc set-origin x y)
			      (send dc draw-line (/ client-w 2) 0 (/ client-w 2) client-h)
			      (send dc draw-line 0 (/ client-h 2) client-w (/ client-h 2))
			      (send dc set-origin 0 0))
			    (format "~a | ~a | ~a | ~a"
				    (or (slide-title a) untitled)
				    (or (slide-title b) untitled)
				    (or (slide-title c) untitled)
				    (or (slide-title d) untitled))
			    #f
			    (slide-page a)
			    (- (+ (slide-page d) (slide-page-count d)) (slide-page a)))
			   (loop (cddddr l))))]))))
  
  (define TALK-MINUTES 60)
  (define GAUGE-WIDTH 100)
  (define GAUGE-HEIGHT 4)

  (define talk-frame%
    (class100 frame% (closeble?)
      (private-field [closeable? closeble?])
      (override
	[can-close? (lambda () closeable?)]
	[on-subwindow-char
	 (lambda (w e)
	   (let ([k (send e get-key-code)])
	     (case k
	       [(right #\space #\f #\n)
		(set! current-page (min (add1 current-page)
					(sub1 (length talk-slide-list))))
		(refresh-page)
		#t]
	       [(left #\b)
		(set! current-page (max (sub1 current-page)
					0))
		(refresh-page)
		#t]
	       [(#\g)
		(if (send e get-meta-down)
		    (get-page-from-user)
		    (begin
		      (set! current-page (sub1 (length talk-slide-list)))
		      (refresh-page)))
		#t]
	       [(#\1)
		(set! current-page 0)
		(refresh-page)
		#t]
	       [(#\q)
		(when (send e get-meta-down)
		  (send c-frame show #f)
		  (send f show #f))
		#f]
	       [else
		#f])))])
      (sequence
	(super-init))))

  (define f (instantiate talk-frame% (#f)
			 [label "Talk"]
			 [x 0] [y 0]
			 [width (inexact->exact (floor screen-w))]
			 [height (inexact->exact (floor screen-h))]
			 [style '(no-caption no-resize-border)]))

  (define c-frame (instantiate talk-frame% (#t) [label "Commentary"] [width 400] [height 100]))
  (define commentary (make-object text%))
  (send (make-object editor-canvas% c-frame commentary)
	set-line-count 3)

  (define start-time #f)

  (define clear-brush (make-object brush% "WHITE" 'transparent))
  (define gray-brush (make-object brush% "GRAY" 'solid))
  (define green-brush (make-object brush% "GREEN" 'solid))
  (define red-brush (make-object brush% "RED" 'solid))
  (define black-pen (make-object pen% "BLACK" 1 'solid))
  (define red-color (make-object color% "RED"))
  (define green-color (make-object color% "GREEN"))
  (define black-color (make-object color% "BLACK"))

  (define (slide-page-string slide)
    (if (= 1 (slide-page-count slide))
	(format "~a" (slide-page slide))
	(format "~a-~a" (slide-page slide) (+ (slide-page slide)
					      (slide-page-count slide)
					      -1))))

  (define (calc-progress)
    (if start-time
	(values (min 1 (/ (- (current-seconds) start-time) (* 60 TALK-MINUTES)))
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

  (define c% (class100 canvas% args
	       (inherit get-dc get-client-size)
	       (private-field
		 [number-font (make-object font% 10 'default 'normal 'normal)])
	       (override
		 [on-paint
		  (lambda ()
		    (let* ([dc (get-dc)]
			   [f (send dc get-font)]
			   [c (send dc get-text-foreground)]
			   [slide (list-ref talk-slide-list current-page)]
			   [s (slide-page-string slide)])
		      (let*-values ([(cw ch) (get-client-size)]
				    [(m) (- margin (/ (- screen-w cw) 2))])
			((slide-drawer slide) (get-dc) m m))
		      
		      ;; Slide number
		      (send dc set-font number-font)
		      (let-values ([(duration distance) (calc-progress)])
			(send dc set-text-foreground 
			      (cond
			       [printing? black-color]
			       [(<= (- duration 0.1)
				    distance
				    (+ duration 0.1))
				black-color]
			       [(< distance duration) red-color]
			       [else green-color])))
		      (let-values ([(w h d a) (send dc get-text-extent s)]
				   [(cw ch) (if printing?
						(send dc get-size)
						(get-client-size))])
			(when show-page-numbers?
			  (send dc draw-text s (- cw w 10) (- ch h 10))) ; 5+5 border
			(send dc set-font f)
			(send dc set-text-foreground c)

			;; Progress gauge
			(when show-gauge?
			  (unless printing?
			    (show-time dc (- cw 10 w) (- ch 10)))))))])
	       (public
		 [redraw (lambda ()
			   (let ([dc (get-dc)])
			     (send dc clear)
			     (on-paint)))])
	       (sequence
		 (apply super-init args))))

  (define c (make-object c% f))

  (define (refresh-page)
    (when (= current-page 0)
      (set! start-time #f)
      (unless start-time
	(set! start-time (current-seconds))))
    (send c redraw))

  (define (get-page-from-user)
    (let* ([d (make-object dialog% "Goto Page" f 200 250)]
	   [short-slide-list 
	    (let loop ([slides talk-slide-list][n 1][last-title #f])
	      (cond
	       [(null? slides) null]
	       [(and last-title
		     (equal? last-title (slide-title (car slides))))
		(loop (cdr slides) (+ n (slide-page-count (car slides))) last-title)]
	       [else
		(let ([title (or (slide-title (car slides))
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
						 (or (slide-title (car slides))
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
      (send d show #t)))

  (refresh-page)

  (send f show #t)

  (when commentary?
    (send c-frame show #t)
    (message-box "Instructions"
		 (format "Keybindings:~
                      ~n  {Meta,Alt}-Q - quit  << IMPORTANT!~
                      ~n  Right, Space, F or N - next page~
                      ~n  Left, B - prev page~
                      ~n  G - last page~
                      ~n  1 - first page~
                      ~n  {Meta,Alt}-G - select page~
                      ~nAll bindings work in both the display and commentary windows")))

  (when printing?
    (let ([ps-dc (dc-for-text-size)])
      (let loop ([start? #f][l (list-tail talk-slide-list current-page)][n current-page])
	(unless (null? l)
	  (set! current-page n)
	  (refresh-page)
	  (when start?
	    (send ps-dc start-page))
	  (let ([slide (car l)])
	    ((slide-drawer slide) ps-dc margin margin)
	    (when show-page-numbers?
	      (let ([s (slide-page-string slide)])
		(let-values ([(w h) (send ps-dc get-size)]
			     [(sw sh sd sa) (send ps-dc get-text-extent s)]
			     [(hm vm) (values margin margin)])
		  (send ps-dc draw-text s (- w hm sw) (- h vm sh))))))
	  (send ps-dc end-page)
	  (loop #t (cdr l) (add1 n))))
      (send ps-dc end-doc))))
