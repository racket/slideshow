
(module slideshow mzscheme
  (require (lib "class.ss")
	   (lib "class100.ss")
           (lib "unit.ss")
	   (lib "file.ss")
	   (lib "etc.ss")
	   (lib "contract.ss")
	   (lib "mred.ss" "mred")
	   (lib "cmdline.ss")
	   (lib "mrpict.ss" "texpict")
	   (lib "utils.ss" "texpict")
	   (lib "math.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                Command Line                   ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-values (screen-w screen-h) (values 1024 768))
  (define base-font-size 32)

  (define-values (actual-screen-w actual-screen-h) (get-display-size #t))

  (define condense? #f)
  (define printing? #f)
  (define commentary? #f)
  (define show-gauge? #f)
  (define keep-titlebar? #f)
  (define show-page-numbers? #t)
  (define quad-view? #f)
  (define print-slide-seconds? #f)
  (define use-transitions? #t)
  (define talk-duration-minutes 25)

  (define current-page 0)
  
  (define content
    (command-line
     "slideshow"
     (current-command-line-arguments)
     [once-each
      (("-p" "--print") "print"
		   (set! printing? #t))
      (("-c" "--condense") "condense"
			   (set! condense? #t))
      (("-t" "--start") page "set the starting page"
       (let ([n (string->number page)])
	 (unless (and n 
		      (integer? n)
		      (exact? n)
		      (positive? n))
	   (error 'slideshow "argument to -t is not a positive exact integer: ~a" page))
	 (set! current-page (sub1 n))))
      (("-q" "--quad") "show four slides at a time"
		       (set! quad-view? #t))
      (("-n" "--no-stretch") "don't stretch the slide window to fit this screen"
       (when (> actual-screen-w screen-w)
	 (set! keep-titlebar? #t)
	 (set! actual-screen-w screen-w)
	 (set! actual-screen-h screen-h)))
      (("-s" "--size") w h "show <w> high and <h> tall"
       (let ([nw (string->number w)]
	     [nh (string->number h)])
	 (unless (and nw (< 0 nw 10000))
	   (error 'slideshow "bad width: ~e" w))
	 (unless (and nw (< 0 nh 10000))
	   (error 'slideshow "bad height: ~e" h))
	 (when (> actual-screen-h nh)
	   (set! keep-titlebar? #t))
	 (set! actual-screen-w nw)
	 (set! actual-screen-h nh)))
      (("-m" "--minutes") min "set talk duration in minutes"
		       (let ([n (string->number min)])
			 (unless (and n 
				      (integer? n)
				      (exact? n)
				      (positive? n))
			   (error 'slideshow "argument to -m is not a positive exact integer: ~a" min))
			 (set! talk-duration-minutes n)))
      (("-i" "--immediate") "no transitions"
       (set! use-transitions? #f))
      (("--comment") "display commentary"
                     (set! commentary? #t))
      (("--time") "time seconds per slide" (set! print-slide-seconds? #t))]
     [args slide-module-file
	   (cond
             [(null? slide-module-file) #f]
             [(null? (cdr slide-module-file)) (car slide-module-file)]
             [else (error 'slideshow
                          "expects at most one module file, given ~a: ~s"
                          (length slide-module-file)
                          slide-module-file)])]))

  (when (or printing? condense?)
    (set! use-transitions? #f))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                    Setup                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define ps-pre-scale 0.7)

  (define font-size base-font-size)
  (define gap-size (* 3/4 font-size))
  (define line-sep 2)
  (define title-size (+ font-size 4))
  (define main-font (if (and (not printing?)
			     (string=? (get-family-builtin-face 'default) " Sans"))
			'default
			'swiss))
  (define current-main-font (make-parameter main-font))

  (when (not (and (= actual-screen-w screen-w)
		  (= actual-screen-h screen-h)))
    (current-expected-text-scale (list (/ actual-screen-w screen-w)
				       (/ actual-screen-h screen-h))))

  (define red "red")
  (define green "forest green")
  (define blue "blue")
  (define purple "purple")
  (define orange "orange")

  (define current-font-size (make-parameter 
			     font-size
			     (lambda (x)
			       (unless (and (number? x)
					    (integer? x)
					    (exact? x)
					    (positive? x))
				 (raise-type-error 'current-font-size "exact non-negative integer" x))
			       x)))

  (define (t s) (text s (current-main-font) (current-font-size)))
  (define (it s) (text s `(italic . ,(current-main-font)) (current-font-size)))
  (define (bt s) (text s `(bold . ,(current-main-font)) (current-font-size)))
  (define (bit s) (text s `(bold italic . ,(current-main-font)) (current-font-size)))
  (define (tt s) (text s '(bold . modern) (current-font-size)))
  (define (rt s) (text s 'roman (current-font-size)))
  (define (titlet s) (colorize (text s 
				     `(bold . ,(current-main-font)) 
				     title-size)
			       green))

  (define (with-font f k)
    (parameterize ([current-main-font f])
      (k)))

  (define (tt* . l) (apply vl-append line-sep (map tt l)))

  (define bullet (cc-superimpose (disk (/ gap-size 2)) 
				 (blank 0 gap-size)))
  (define o-bullet (cc-superimpose (circle (/ gap-size 2)) 
				   (blank 0 gap-size)))

  (dc-for-text-size
   (if printing?
       ;; Make ps-dc%:
       (let ([pss (make-object ps-setup%)])
	 (send pss set-mode 'file)
	 (send pss set-file
	       (if content
		   (regexp-replace "[.][^.]+$" (file-name-from-path content) 
				   (if quad-view?
				       "-4u.ps"
				       ".ps"))
		   "untitled.ps"))
	 (send pss set-scaling ps-pre-scale ps-pre-scale)
	 (send pss set-orientation 'landscape)
	 (parameterize ([current-ps-setup pss])
	   (let ([p (make-object post-script-dc% #t #f #t)])
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
  (define (mk-titleless-page)
    (inset full-page 0 (- 0 (pict-height (titlet "Hi")) (* 2 gap-size)) 0 0))
  (define titleless-page (mk-titleless-page))

  (define use-background-frame? #f)
    
  (define (set-use-background-frame! on?)
    (set! use-background-frame? (and on? #t)))

  (define talk-slide-list null)
  (define-struct slide (drawer title comment page page-count inset transitions))
  (define-struct just-a-comment (text))
  (define-struct sinset (l t r b))

  (define page-number 1)

  (define (add-slide! pict title comment page-count inset)
    (set! talk-slide-list (cons
			   (make-slide (make-pict-drawer pict)
				       title 
				       comment
				       page-number
				       page-count
				       inset
				       null)
			   talk-slide-list))
    (set! page-number (+ page-number page-count))
    (send progress-display set-label (number->string page-number)))

  (define (skip-slides n)
    (set! page-number (+ page-number n)))

  (define (evenize-width p)
    (let ([w (pict-width p)])
      ;; Force even size:
      (inset p 0 0 (+ (- w (floor w)) (modulo (floor w) 2)) 0)))

  (define (apply-slide-inset sinset pict)
    (inset pict 
	   (- (sinset-l sinset))
	   (- (sinset-t sinset))
	   (- (sinset-r sinset))
	   (- (sinset-b sinset))))

  (define (do-add-slide! content title comment page-count inset)
    (add-slide!
     (ct-superimpose
      (apply-slide-inset inset full-page)
      content)
     title
     comment
     page-count
     inset))

  (define (one-slide/title/inset do-add-slide! process v-sep skipped-pages s inset . x) 
    (let-values ([(x c)
		  (let loop ([x x][c #f][r null])
		    (cond
		     [(null? x) (values (reverse! r) c)]
		     [(just-a-comment? (car x))
		      (loop (cdr x) (car x) r)]
		     [else
		      (loop (cdr x) c (cons (car x) r))]))])
      (let ([content (apply vc-append v-sep
			    (map
			     evenize-width
			     (if s
				 (cons (titlet s) (process x))
				 (process x))))])
	(do-add-slide!
	 content
	 s
	 c
	 (+ 1 skipped-pages)
	 inset))))

  (define (slide-error nested string . args)
    (apply error
	   (let loop ([nested nested])
	     (if (null? nested)
		 'slide*
		 (string->symbol (format "~a of ~a" (car nested) (loop (cdr nested))))))
	   string
	   args))

  (define (do-slide/title/tall/inset do-add-slide! skip-ok? process v-sep s inset . x)
    ;; Check slides:
    (let loop ([l x][nested null])
      (or (null? l)
	  (cond
	   [(pict? (car l)) (loop (cdr l) nested)]
	   [(just-a-comment? (car l)) (loop (cdr l) nested)]
	   [(memq (car l) '(next next!)) (and (or (pair? l) 
						  (slide-error nested "argument sequence contains 'next at end"))
					      (loop (cdr l) nested))]
	   [(memq (car l) '(alts alts~)) (and (or (pair? (cdr l))
						  (slide-error nested "argument sequence contains '~a at end" (car l)))
					      (let ([a (cadr l)])
						(and (or (list? a)
							 (slide-error nested "non-list after '~a: ~e" (car l) a))
						     (andmap (lambda (sl)
							       (loop sl (cons (car l) nested)))
							     a)))
					      (loop (cddr l) nested))]
	   [(eq? (car l) 'nothing) (loop (cdr l) nested)]
	   [else #f])
	  (slide-error nested "argument sequence contains a bad element: ~e" (car l))))

    (let loop ([l x][r null][comment #f][skip-all? #f][skipped 0])
      (cond
       [(null? l) 
	(if skip-all?
	    (add1 skipped)
	    (begin
	      (apply one-slide/title/inset do-add-slide! process v-sep skipped s inset (reverse r))
	      0))]
       [(memq (car l) '(NOTHING))
	(loop (cdr l) r comment skip-all? skipped)]
       [(memq (car l) '(NEXT NEXT!))
	(let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'NEXT)))])
	  (let ([skipped (if skip?
			     (add1 skipped)
			     (begin
			       (apply one-slide/title/inset do-add-slide! process v-sep skipped s inset (reverse r))
			       0))])
	    (loop (cdr l) r comment skip-all? skipped)))]
       [(memq (car l) '(ALTS ALTS~)) 
	(let ([rest (cddr l)])
	  (let aloop ([al (cadr l)][skipped skipped])
	    (if (null? (cdr al))
		(loop (append (car al) rest) r comment skip-all? skipped)
		(let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'ALTS~)))])
		  (let ([skipped (loop (car al) r comment skip? skipped)])
		    (aloop (cdr al) skipped))))))]
       [else (loop (cdr l) (cons (car l) r) comment skip-all? skipped)])))

  ;; Let the contract check always pass. We do more specific checking.
  (define (slide-sequence? l) #t)

  (define zero-inset (make-sinset 0 0 0 0))

  (define (side-inset? n) (and (number? n)
			       (exact? n)
			       (integer? n)
			       (n . >= . 0)))
  (define (make-slide-inset l t r b)
    (make-sinset l t r b))

  (define (slide/title/tall/inset s inset . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title/tall/inset "string" s))
    (unless (sinset? inset)
      (raise-type-error 'slide/title/tall/inset "slide-inset" inset))
    (apply do-slide/title/tall/inset do-add-slide! #t values gap-size s inset x))

  (define (slide/title/tall s . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title/tall "string" s))
    (apply do-slide/title/tall/inset do-add-slide! #t values gap-size s zero-inset x))

  (define (slide/title s . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title "string" s))
    (if s
	(apply slide/title/tall s (blank) x)
	(apply slide/title/tall s x)))

  (define (slide/title/inset s inset . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title "string" s))
    (apply slide/title/tall/inset s inset (blank) x))

  (define (slide/title/center/inset s inset . x)
    (unless (or (string? s) (not s))
      (raise-type-error 'slide/title/center "string" s))
    (unless (sinset? inset)
      (raise-type-error 'slide/title/center/inset "slide-inset" inset))
    (let ([max-width 0]
	  [max-height 0]
	  [combine (lambda (x)
		     (apply vc-append gap-size
			    (map
			     evenize-width
			     x)))])
      ;; Run through all the slides once to measure (don't actually create slides):
      (apply do-slide/title/tall/inset
	     (lambda (content title comment page-count inset)
	       (set! max-width (max max-width (pict-width content)))
	       (set! max-height (max max-height (pict-height content))))
	     #f
	     (lambda (x) (list (combine x)))
	     0 #f inset x)
      (apply do-slide/title/tall/inset
	     do-add-slide!
	     #t
	     (lambda (x)
	       (list
		(cc-superimpose
		 (apply-slide-inset inset (if s titleless-page full-page))
		 (ct-superimpose
		  (blank max-width max-height)
		  (combine x)))))
	     0 s inset x)))

  (define (slide/title/center s . x)
    (apply slide/title/center/inset s zero-inset x))

  (define (:slide . x) (apply slide/title #f x))
  (define (slide/inset inset . x) (apply slide/title/inset #f inset x))

  (define (slide/center . x) (apply slide/title/center #f x))
  (define (slide/center/inset inset . x) (apply slide/title/center/inset #f inset x))

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

  (define re-slide
    (opt-lambda (s [addition #f])
      (unless (slide? s)
	(raise-type-error 're-slide "slide" s))
      (set! talk-slide-list (cons (make-slide
				   (let ([orig (slide-drawer s)]
					 [extra (if addition
						    (make-pict-drawer addition)
						    void)])
				     (lambda (dc x y)
				       (orig dc x y)
				       (extra dc x y)))
				   (slide-title s)
				   (slide-comment s)
				   page-number
				   1
				   (slide-inset s)
				   null)
				  talk-slide-list))
      (set! page-number (+ page-number 1))))

  (define (make-outline . l)
    (define ah (arrowhead gap-size 0))
    (define current-item (colorize (hc-append (- (/ gap-size 2)) ah ah) blue))
    (define other-item (rc-superimpose (ghost current-item) (colorize ah "light gray")))
    (lambda (which)
      (slide/title
       "Outline"
       (lc-superimpose
	(blank (pict-width full-page) 0)
	(let loop ([l l])
	  (cond
	   [(null? l) (blank)]
	   [else
	    (let ([current? (or (eq? which (car l)) 
				(and (list? (car l))
				     (memq which (car l))))])
	      (vc-append
	       gap-size
	       (page-para
		(hbl-append
		 (quotient gap-size 2)
		 (if current?
		     current-item
		     other-item)
		 (let ([p (cadr l)])
		   (if (pict? p)
		       p
		       (bt p)))))
	       (let ([rest (loop (cdddr l))]
		     [sub-items (caddr l)])
		 (if (and current?
			  sub-items
			  (not (null? sub-items)))
		     (vc-append
		      gap-size
		      (sub-items which)
		      rest)
		     rest))))]))))))

  (define (comment . s) (make-just-a-comment
			 (apply string-append s)))

  ;; ----------------------------------------

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
	(let* ([sep? (and (string? s) (regexp-match "^[',. :;-?!]" s))]
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


  (define (para/align superimpose v-append w . s)
    (superimpose (para*/align v-append w s)
		 (blank w 0)))

  (define (para w . s)
    (para/align lbl-superimpose vl-append w s))

  (define (para/r w . s)
    (para/align rbl-superimpose vr-append w s))

  (define (para/c w . s)
    (para/align cbl-superimpose vc-append w s))


  (define (page-para*/align v-append . s)
    (para*/align v-append client-w s))

  (define (page-para* . s)
    (page-para*/align vl-append s))

  (define (page-para*/r . s)
    (page-para*/align vr-append s))

  (define (page-para*/c . s)
    (page-para*/align vc-append s))


  (define (page-para/align superimpose v-append . s)
    (para/align superimpose v-append client-w s))

  (define (page-para . s)
    (page-para/align lbl-superimpose vl-append s))

  (define (page-para/r . s)
    (page-para/align rbl-superimpose vr-append s))

  (define (page-para/c . s)
    (page-para/align cbl-superimpose vc-append s))

  ;; ----------------------------------------

  (define (l-combiner para w l)
    (apply
     vl-append
     gap-size
     (map (lambda (x) (para w x)) l)))

  ;; ----------------------------------------

  (define (item*/bullet bullet w . s)
    (htl-append (/ gap-size 2)
		bullet 
		(para* (- w
			  (pict-width bullet) 
			  (/ gap-size 2)) 
		       s)))

  (define (item* w . s)
    (apply item*/bullet bullet w s))

  (define (item w . s)
    (lbl-superimpose (item* w s)
		     (blank w 0)))

  (define (item/bullet b w . s)
    (lbl-superimpose (item*/bullet b w s)
		     (blank w 0)))

  (define (page-item* . s)
    (item* client-w s))

  (define (page-item . s)
    (item client-w s))

  (define (page-item*/bullet b . s)
    (item*/bullet b client-w s))

  (define (page-item/bullet b . s)
    (item/bullet b client-w s))

  ;; ----------------------------------------

  (define (subitem* w . s)
    (inset (htl-append (/ gap-size 2)
		       o-bullet 
		       (para* (- w
				 (* 2 gap-size)
				 (pict-width bullet) 
				 (/ gap-size 2)) 
			      s))
	   (* 2 gap-size) 0 0 0))

  (define (subitem w . s)
    (lbl-superimpose (subitem* w s)
		     (blank w 0)))

  (define (page-subitem* . s)
    (subitem* client-w s))

  (define (page-subitem . s)
    (subitem client-w s))

  ;; ----------------------------------------

  (define (paras* w . l)
    (l-combiner para* w l))

  (define (paras w . l)
    (l-combiner para w l))

  (define (page-paras* . l)
    (l-combiner (lambda (x y) (page-para* y)) client-w l))

  (define (page-paras . l)
    (l-combiner (lambda (x y) (page-para y)) client-w l))

  ;; ----------------------------------------

  (define (itemize w . l)
    (l-combiner item w l))

  (define (itemize* w . l)
    (l-combiner item* w l))

  (define (page-itemize . l)
    (l-combiner (lambda (x y) (page-item y)) client-w l))

  (define (page-itemize* . l)
    (l-combiner (lambda (x y) (page-item* y)) client-w l))

  ;; ----------------------------------------

  (define (size-in-pixels p)
    (if (not (and (= actual-screen-w screen-w)
		  (= actual-screen-h screen-h)))
	(scale p 
	       (/ screen-w actual-screen-w)
	       (/ screen-h actual-screen-h))
	p))

  ;; ----------------------------------------

  (define-struct click-region (left top right bottom thunk show-click?))

  (define click-regions null)

  (define clickback
    (opt-lambda (pict thunk [show-click? #t])
      (let ([w (pict-width pict)]
	    [h (pict-height pict)])
	(cons-picture*
	 pict
	 `((place 
	    0 0
	    ,(dc (lambda (dc x y)
		   (let-values ([(sx sy) (send dc get-scale)]
				[(dx dy) (send dc get-origin)])
		     (set! click-regions
			   (cons
			    (make-click-region (+ (* x sx) dx)
					       (+ (* y sy) dy)
					       (+ (* (+ x w) sx) dx)
					       (+ (* (+ y h) sy) dy)
					       thunk
					       show-click?)
			    click-regions))))
		 w h
		 (pict-ascent pict)
		 (pict-descent pict))))))))

  ;; ----------------------------------------

  (define (add-transition! who trans)
    (unless (or (null? talk-slide-list)
		(not use-transitions?))
      (let ([slide (car talk-slide-list)])
	(set-slide-transitions! slide 
				(append! (slide-transitions slide)
					 (list trans))))))

  (define scroll-bm #f)
  (define scroll-dc (make-object bitmap-dc%))

  ;; This code currently works only with negative displacements
  (define scroll-transition
    (opt-lambda (x y w h dx dy [duration 0.20] [steps 12])
      (add-transition! 'scroll-transition
		       (lambda (offscreen-dc)
			 (let ([steps-done 0]
			       [xs (/ actual-screen-w screen-w)]
			       [ys (/ actual-screen-h screen-h)])
			   (unless (and scroll-bm
					(>= (send scroll-bm get-width) 
					    (+ w dx))
					(>= (send scroll-bm get-height) 
					    (+ h dy)))
			     (set! scroll-bm (make-object bitmap% 
							  (inexact->exact (ceiling (+ w (abs dx))))
							  (inexact->exact (ceiling (+ h (abs dy))))))
			     (if (send scroll-bm ok?)
				 (send scroll-dc set-bitmap scroll-bm)
				 (set! scroll-bm #f)))

			   (when scroll-bm
			     (send scroll-dc clear)
			     (send scroll-dc draw-bitmap-section (send offscreen-dc get-bitmap)
				   0 0
				   (* (+ margin x) xs) (* (+ margin y) ys)
				   (* w xs) (* h ys)))
			   
			   (lambda (canvas offscreen-dc)
			     (if (or (not scroll-bm) (= steps-done steps))
				 'done
				 (let*-values ([(cw ch) (send canvas get-client-size)]
					       [(xm) (- (/ (- actual-screen-w cw) 2))]
					       [(ym) (- (/ (- actual-screen-h ch) 2))])
				   (set! steps-done (add1 steps-done))
				   (let ([draw
					  (lambda (dc xm ym)
					    (send dc draw-bitmap-section
						  scroll-bm
						  (+ (* xs (+ x margin (* dx (/ steps-done steps)))) xm)
						  (+ (* ys (+ y margin (* dy (/ steps-done steps)))) ym)
						  0 0 
						  (ceiling (* xs (+ w (/ (abs dx) steps))))
						  (ceiling (* ys (+ h (/ (abs dy) steps))))))])
				     (draw (send canvas get-dc) xm ym)
				     (draw offscreen-dc 0 0))
				   (/ duration steps)))))))))

  (define pause-transition
    (lambda (time)
      (add-transition! 'pause-transition
		       (lambda (offscreen-dc)
			 (let ([done? #f])
			   (lambda (canvas offscreen-dc)
			     (if done?
				 'done
				 (begin
				   (set! done? #t)
				   time))))))))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                Contracts                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define slide-contract
    (() slide-sequence? . ->* . any))
  (define slide/title-contract
    ((string?) slide-sequence? . ->* . any))
  (define slide/inset-contract
    ((sinset?) slide-sequence? . ->* . any))
  (define slide/title/inset-contract
    ((string? sinset?) slide-sequence? . ->* . any))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                 Talk                          ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define-syntax (provide-slide stx)
    #'(begin
       ;; The name slide is introduced by this macro,
       ;;  so it doesn't conflict with the real `slide'!
       (define slide :slide)
       (provide/contract [slide slide-contract])))

  (provide-slide)
  (provide (all-from mzscheme))
  (provide/contract [slide/title slide/title-contract]
		    [slide/title/tall slide/title-contract]
		    [slide/center slide-contract]
		    [slide/title/center slide/title-contract]
		    [slide/inset slide/inset-contract]
		    [slide/title/inset slide/title/inset-contract]
		    [slide/title/tall/inset slide/title/inset-contract]
		    [slide/center/inset slide/inset-contract]
		    [slide/title/center/inset slide/title/inset-contract])
  (provide most-recent-slide retract-most-recent-slide re-slide 
	   scroll-transition pause-transition
	   comment make-outline
	   item item* page-item page-item*
	   item/bullet item*/bullet page-item/bullet page-item*/bullet
	   subitem subitem* page-subitem page-subitem*
	   itemize itemize* page-itemize page-itemize*
	   para para* page-para page-para*
	   para/c para/r para*/c para*/r page-para/c page-para/r page-para*/c page-para*/r
	   font-size gap-size current-font-size line-sep title-size 
	   main-font current-main-font with-font
	   red green blue purple orange size-in-pixels
	   t it bt bit tt titlet tt* rt
	   bullet o-bullet
	   margin client-w client-h
	   full-page titleless-page
	   printing? condense? skip-slides
	   set-use-background-frame!
	   (all-from (lib "mrpict.ss" "texpict"))
	   (all-from (lib "utils.ss" "texpict"))
           start-making-slides done-making-slides)
  (provide/contract [clickback 
		     (pict? (lambda (x)
			      (and (procedure? x)
				   (procedure-arity-includes? x 0)))
			    . -> .
			    pict?)]
		    [make-slide-inset
		     (side-inset? side-inset? side-inset? side-inset?
				  . -> .
				  sinset?)]
		    [apply-slide-inset
		     (sinset? pict? . -> . pict?)])
  
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
      
  (define go@
    (unit
      (import)
      (export)
      
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
                                     (make-slide void #f #f page-number 1 zero-inset null)))))]
                  [else (let ([a (car l)]
                              [b (cadr l)]
                              [c (caddr l)]
                              [d (cadddr l)]
                              [untitled "(untitled)"])
                          (cons (make-slide
                                 (lambda (dc x y)
				   (define-values (orig-sx orig-sy) (send dc get-scale))
                                   (define scale (min (/ (- (/ client-h 2) margin) client-h)
                                                      (/ (- (/ client-w 2) margin) client-w)))
                                   (send dc set-scale (* orig-sx scale) (* orig-sy scale))
                                   (send dc set-origin x y)
                                   ((slide-drawer a) dc 0 0)
                                   (send dc set-origin (+ x (/ client-w 2) margin) y)
                                   ((slide-drawer b) dc 0 0)
                                   (send dc set-origin x (+ y (/ client-h 2) margin))
                                   ((slide-drawer c) dc 0 0)
                                   (send dc set-origin (+ x (/ client-w 2) margin) (+ y (/ client-h 2) margin))
                                   ((slide-drawer d) dc 0 0)
                                   (send dc set-scale orig-sx orig-sy)
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
                                 (- (+ (slide-page d) (slide-page-count d)) (slide-page a))
				 zero-inset
				 null)
                                (loop (cddddr l))))]))))
      
      (define GAUGE-WIDTH 100)
      (define GAUGE-HEIGHT 4)

      (define talk-start-seconds (current-seconds))
      (define slide-start-seconds (current-seconds))
      
      (define talk-frame%
        (class100 frame% (closeble?)
          (private-field [closeable? closeble?])
          (override
            [can-close? (lambda () closeable?)]
	    [on-superwindow-show (lambda (on?)
				   (unless on?
				     (when background-f
				       (send background-f show #f))))]
            [on-subwindow-char
             (lambda (w e)
               (let ([k (send e get-key-code)])
                 (case k
                   [(right #\space #\f #\n)
		    (if (pair? current-transitions)
			(stop-transition)
			(let ([old (list-ref talk-slide-list current-page)])
			  (set! current-page (min (add1 current-page)
						  (sub1 (length talk-slide-list))))
			  (when print-slide-seconds?
			    (let ([slide-end-seconds (current-seconds)])
			      (printf "Slide ~a: ~a seconds~n" current-page
				      (- slide-end-seconds slide-start-seconds))
			      (set! slide-start-seconds slide-end-seconds)))
			  (do-transitions (slide-transitions old) (send c get-offscreen))
			  #t))]
                   [(left #\b)
		    (stop-transition)
                    (set! current-page (max (sub1 current-page)
                                            0))
                    (refresh-page)
                    #t]
                   [(#\g)
		    (stop-transition)
                    (if (send e get-meta-down)
                        (get-page-from-user)
                        (begin
                          (set! current-page (sub1 (length talk-slide-list)))
                          (refresh-page)))
                    #t]
                   [(#\1)
		    (stop-transition)
                    (set! current-page 0)
                    (refresh-page)
                    #t]
                   [(#\q #\S)  ; #\S is for Mac OS
		    (stop-transition)
                    (when (or (send e get-meta-down)
			      (send e get-alt-down))
                      (send c-frame show #f)
                      (send f show #f))
		    (when print-slide-seconds?
		      (printf "Total Time: ~a seconds~n"
			(- (current-seconds) talk-start-seconds)))
                    #f]
                   [else
                    #f])))])
          (sequence
            (super-init))))
      
      (define-values (screen-left-inset screen-top-inset)
	(get-display-left-top-inset))

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
			     [width (inexact->exact (floor actual-screen-w))]
			     [height (inexact->exact (floor actual-screen-h))]
			     [style '(no-caption no-resize-border hide-menu-bar)])))))

      (when background-f
	(send background-f enable #f)
	(send background-f show #t))

      (define f (instantiate talk-frame% (#f)
                  [label (if content
                             (format "~a: slideshow" (file-name-from-path content))
                             "Slideshow")]
                  [x (- screen-left-inset)] [y (- screen-top-inset)]
                  [width (inexact->exact (floor actual-screen-w))]
                  [height (inexact->exact (floor actual-screen-h))]
                  [style (if keep-titlebar?
			     null
			     '(no-caption no-resize-border hide-menu-bar))]))
      
      (define current-sinset zero-inset)
      (define (reset-display-inset! sinset)
	(unless (and (= (sinset-l current-sinset) (sinset-l sinset))
		     (= (sinset-t current-sinset) (sinset-t sinset))
		     (= (sinset-r current-sinset) (sinset-r sinset))
		     (= (sinset-b current-sinset) (sinset-b sinset)))
	  (send f resize 
		(max 1 (- (inexact->exact (floor actual-screen-w)) (sinset-l sinset) (sinset-r sinset)))
		(max 1 (- (inexact->exact (floor actual-screen-h)) (sinset-t sinset) (sinset-b sinset))))
	  (send f move (sinset-l sinset) (sinset-t sinset))
	  (set! current-sinset sinset)))
		

      (define c-frame (instantiate talk-frame% (#t) [label "Commentary"] [width 400] [height 100]))
      (define commentary (make-object text%))
      (send (make-object editor-canvas% c-frame commentary)
            set-line-count 3)
      
      (define start-time #f)
      
      (define clear-brush (make-object brush% "WHITE" 'transparent))
      (define white-brush (make-object brush% "WHITE" 'solid))
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
            (values (min 1 (/ (- (current-seconds) start-time) (* 60 talk-duration-minutes)))
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
                   (private
                     [paint
                      (lambda (dc)
			(stop-transition/no-refresh)
                        (let* ([f (send dc get-font)]
                               [c (send dc get-text-foreground)]
                               [slide (list-ref talk-slide-list current-page)]
                               [s (slide-page-string slide)]
			       [set-scale? (not (and (= actual-screen-w screen-w)
						     (= actual-screen-h screen-h)))])
			  ;; Scale to adjust for screen size
			  (when set-scale?
			    (send dc set-scale 
				  (/ actual-screen-w screen-w)
				  (/ actual-screen-h screen-h)))
			    
			  ;; Draw the slide
                          (let*-values ([(cw ch) (get-client-size)]
                                        [(m) (- margin (/ (- actual-screen-w cw) 2))])
                            ((slide-drawer slide) dc m m))

			  ;; Reset the scale:
                          (when set-scale?
			    (send dc set-scale 1 1))

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

		   (private-field
		     [clicking #f]
		     [clicking-hit? #f])

		   (override
		     [on-paint (lambda () (paint (get-dc)))]
		     [on-event (lambda (e)
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
				  [else 
				   (when (and clicking clicking-hit?)
				     (invert-clicking!))
				   (set! clicking #f)]))])
		   
		   (private
		     [click-hits?
		      (lambda (e c)
			(let ([x (send e get-x)]
			      [y (send e get-y)])
			  (and (<= (click-region-left c) x (click-region-right c))
			       (<= (click-region-top c) y (click-region-bottom c)))))]
		     [invert-clicking!
		      (lambda ()
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
			  (send dc set-brush b)))])
		   
		   (private-field
		    [offscreen #f])
		   (public
		     [get-offscreen (lambda () offscreen)])
		   
                   (public
                     [redraw (lambda ()
			       (reset-display-inset! (slide-inset (list-ref talk-slide-list current-page)))
			       (cond
				[use-transitions?
				 (let-values ([(cw ch) (get-client-size)])
				   (when (and offscreen
					      (let ([bm (send offscreen get-bitmap)])
						(not (and (= cw (send bm get-width))
							  (= ch (send bm get-height))))))
				     (set! offscreen #f))
				   (unless offscreen
				     (set! offscreen (make-object bitmap-dc% 
								  (make-object bitmap% cw ch)))))
				 (set! click-regions null)
				 (set! clicking #f)
				 (send offscreen clear)
				 (paint offscreen)
				 (let ([bm (send offscreen get-bitmap)])
				   (send (get-dc) draw-bitmap bm 0 0))]
				[else
				 (let ([dc (get-dc)])
				   (send dc clear)
				   (set! click-regions null)
				   (paint dc))]))])
                   (sequence
                     (apply super-init args))))
      
      (define c (make-object c% f))
      
      (define (refresh-page)
        (when (= current-page 0)
          (set! start-time #f)
          (unless start-time
            (set! start-time (current-seconds))))
        (send c redraw))

      (define current-transitions null)
      (define current-transitions-key #f)

      (define (do-transitions transes offscreen)
	(let ([key (cons 1 2)])
	  (set! current-transitions (map (lambda (mk) (mk offscreen)) transes))
	  (set! current-transitions-key key)
	  (if (null? transes)
	      (refresh-page)
	      (let do-trans ()
		(when (and (eq? current-transitions-key key)
			   (pair? current-transitions))
		  (let ([went ((car current-transitions) c offscreen)])
		    (if (eq? went 'done)
			(begin
			  (set! current-transitions (cdr current-transitions))
			  (if (null? current-transitions)
			      (refresh-page)
			      (do-trans)))
			(new timer% 
			     [just-once? #t]
			     [interval (inexact->exact (floor (* 1000 went)))]
			     [notify-callback do-trans]))))))))

      (define (stop-transition)
	(stop-transition/no-refresh)
	(refresh-page))
      
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
      
      (let ([bm (make-object bitmap% (build-path (collection-path "slideshow") "slideshow.bmp"))]
            [mbm (make-object bitmap% (build-path (collection-path "slideshow") "mask.xbm"))])
        (when (send bm ok?)
          (send f set-icon bm (and (send mbm ok?) mbm) 'both)))
      
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
          (send ps-dc end-doc)))))

  (define done-once? #f)
  (define making-nesting-depth 0)
  
  (define (start-making-slides)
    (set! making-nesting-depth (add1 making-nesting-depth)))
  (define (done-making-slides)
    (if done-once?
        (message-box "Slideshow"
                     "Is more than one module using the slideshow-run language?")
        (begin
          (set! making-nesting-depth (sub1 making-nesting-depth))
          (when (zero? making-nesting-depth)
            (set! done-once? #t)
            (send progress-window show #f)
            (unless (null? talk-slide-list)
              (invoke-unit go@))))))
  
  (when content
    (start-making-slides)
    (dynamic-require `(file ,content) #f)
    (done-making-slides)))
