
(module slideshow mzscheme
  (require (lib "class.ss")
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
  (define use-offscreen? #t)
  (define use-transitions? use-offscreen?)
  (define talk-duration-minutes #f)
  (define no-squash? #t)
  (define two-frames? #f)
  (define use-prefetch? #t)
  (define use-prefetch-in-preview? #f)
  (define print-target #f)
  
  (define current-page 0)
  
  (define content
    (command-line
     "slideshow"
     (current-command-line-arguments)
     [once-each
      (("-d" "--preview") "show next-slide preview (useful on a non-mirroring display)" 
       (set! two-frames? #t))
      (("-p" "--print") "print"
       (set! printing? #t))
      (("-o") file "set output file for printing"
       (set! print-target file))
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
      (("-n" "--no-stretch") "don't stretch the slide window to fit the screen"
       (when (> actual-screen-w screen-w)
	 (set! actual-screen-w screen-w)
	 (set! actual-screen-h screen-h)))
      (("-s" "--size") w h "use a <w> by <h> window"
       (let ([nw (string->number w)]
	     [nh (string->number h)])
	 (unless (and nw (< 0 nw 10000))
	   (error 'slideshow "bad width: ~e" w))
	 (unless (and nw (< 0 nh 10000))
	   (error 'slideshow "bad height: ~e" h))
	 (set! actual-screen-w nw)
	 (set! actual-screen-h nh)))
      (("-a" "--squash") "scale to full window, even if not 4:3 aspect"
       (set! no-squash? #f))
      ;; Disable --minutes, because it's not used
      #;
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
      (("--no-prefetch") "disable next-slide prefetch"
       (set! use-prefetch? #f))
      (("--preview-prefetch") "use prefetch for next-slide preview"
       (set! use-prefetch-in-preview? #t))
      (("--keep-titlebar") "give the slide window a title bar and resize border"
       (set! keep-titlebar? #t))
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

  (when printing?
    (set! use-offscreen? #f)
    (set! use-prefetch? #f)
    (set! keep-titlebar? #t))

  (dc-for-text-size
   (if printing?
       ;; Make ps-dc%:
       (let ([pss (make-object ps-setup%)])
         (send pss set-mode 'file)
	 (send pss set-file
	       (if print-target
		   print-target
		   (if content
		       (path-replace-suffix (file-name-from-path content) 
					    (if quad-view?
						"-4u.ps"
						".ps"))
		       "untitled.ps")))
	 (send pss set-orientation 'landscape)
	 (parameterize ([current-ps-setup pss])
	   (let ([p (make-object post-script-dc% (not print-target) #f #t #f)])
	     (unless (send p ok?) (exit))
	     (send p start-doc "Slides")
	     (send p start-page)
	     (set!-values (actual-screen-w actual-screen-h) (send p get-size))
	     p)))

       ;; Bitmaps give same size as the screen:
       (make-object bitmap-dc% (make-object bitmap% 1 1))))

  (define-values (use-screen-w use-screen-h)
    (if no-squash?
	(if (< (/ actual-screen-w screen-w)
	       (/ actual-screen-h screen-h))
	    (values actual-screen-w
		    (floor (* (/ actual-screen-w screen-w) screen-h)))
	    (values (floor (* (/ actual-screen-h screen-h) screen-w))
		    actual-screen-h))
	(values actual-screen-w actual-screen-h)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                    Setup                      ;;
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define font-size base-font-size)
  (define gap-size (* 3/4 font-size))
  (define line-sep 2)
  (define title-size (+ font-size 4))
  (define main-font (if (and (not printing?)
			     (string=? (get-family-builtin-face 'default) " Sans"))
			'default
			'swiss))
  (define current-main-font (make-parameter main-font))

  (when (not (and (= use-screen-w screen-w)
		  (= use-screen-h screen-h)))
    (current-expected-text-scale (list (/ use-screen-w screen-w)
				       (/ use-screen-h screen-h))))

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

  (define current-title-color (make-parameter
			       green
			       (lambda (x)
				 (unless (or (string? x)
					     (x . is-a? . color%))
				   (raise-type-error 'current-title-color
						     "string or color% object"
						     x))
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
			       (current-title-color)))

  (define (with-font f k)
    (parameterize ([current-main-font f])
      (k)))

  (define (tt* . l) (apply vl-append line-sep (map tt l)))

  (define bullet (if (send (dc-for-text-size) glyph-exists? #\u2022)
		     (t "\u2022")
		     (cc-superimpose (disk (/ gap-size 2)) 
				     (blank 0 gap-size))))
  (define o-bullet (cc-superimpose (circle (/ gap-size 2)) 
				   (blank 0 gap-size)))


  (define margin 20)
  (define-values (client-w client-h) (values (- screen-w (* margin 2))
					     (- screen-h (* margin 2))))
  (define full-page (blank client-w client-h))
  (define title-h (pict-height (titlet "Hi")))
  (define (mk-titleless-page)
    (inset full-page 0 (- 0 title-h (* 2 gap-size)) 0 0))
  (define titleless-page (mk-titleless-page))

  (define (set-margin! m)
    (set! margin m)
    (set! client-w (- screen-w (* 2 margin)))
    (set! client-h (- screen-h (* 2 margin)))
    (set! full-page (blank client-w client-h))
    (set! titleless-page (mk-titleless-page)))

  (define (set-title-h! h)
    (set! title-h h)
    (set! titleless-page (mk-titleless-page)))

  (define use-background-frame? #f)
    
  (define (set-use-background-frame! on?)
    (set! use-background-frame? (and on? #t)))

  (define click-to-advance? #t)
    
  (define (enable-click-advance! on?)
    (set! click-to-advance? (and on? #t)))

  (define (set-page-numbers-visible! on?)
    (set! show-page-numbers? (and on? #t)))

  (define current-page-number-font 
    (make-parameter
     (make-object font% 10 'default 'normal 'normal)
     (lambda (f)
       (unless (f . is-a? . font%)
	 (raise-type-error 'current-page-number-font "font%" f))
       f)))
  (define current-page-number-color 
    (make-parameter (make-object color% "black")
		    (lambda (s)
		      (unless (s . is-a? . color%)
			(raise-type-error 'current-page-number-color "color%" s))
		      s)))
      
  (define talk-slide-list null)
  (define-struct slide (drawer title comment page page-count inset transitions))
  (define-struct just-a-comment (text))
  (define-struct sinset (l t r b))
  (define-struct name-only (title))

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
      (inset p 0 0 (+ (- (ceiling w) w)
		      (modulo (ceiling w) 2)) 0)))

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

  (define default-slide-assembler
    (lambda (s v-sep p)
      (apply vc-append v-sep
	     (if s
		 (list (evenize-width (titlet s)) p)
		 (list p)))))

  (define current-slide-assembler
    (make-parameter default-slide-assembler))

  (define (one-slide/title/inset do-add-slide! use-assem? process v-sep skipped-pages s inset . x) 
    (let-values ([(x c)
		  (let loop ([x x][c #f][r null])
		    (cond
		     [(null? x) (values (reverse! r) c)]
		     [(just-a-comment? (car x))
		      (loop (cdr x) (car x) r)]
		     [else
		      (loop (cdr x) c (cons (car x) r))]))])
      (let ([content ((if use-assem?
			  (current-slide-assembler)
			  default-slide-assembler)
		      (and (not (name-only? s)) s)
		      v-sep
		      (apply vc-append 
			     gap-size
			     (map evenize-width (process x))))])
	(do-add-slide!
	 content
	 (if (name-only? s) (name-only-title s) s)
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

  (define (do-slide/title/tall/inset do-add-slide! use-assem? skip-ok? process v-sep s inset . x)
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
							       (unless (list? sl)
								 (slide-error nested "non-list in list after '~a: ~e" 
									      (car l) sl))
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
	      (apply one-slide/title/inset do-add-slide! use-assem? process v-sep skipped s inset (reverse r))
	      0))]
       [(memq (car l) '(nothing))
	(loop (cdr l) r comment skip-all? skipped)]
       [(memq (car l) '(next next!))
	(let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'next)))])
	  (let ([skipped (if skip?
			     (add1 skipped)
			     (begin
			       (apply one-slide/title/inset do-add-slide! use-assem? process v-sep skipped s inset (reverse r))
			       0))])
	    (loop (cdr l) r comment skip-all? skipped)))]
       [(memq (car l) '(alts alts~)) 
	(let ([rest (cddr l)])
	  (let aloop ([al (cadr l)][skipped skipped])
	    (cond
	     [(null? al) ;; only happens when al starts out null
	      (loop rest r comment skip-all? skipped)]
	     [(null? (cdr al))
	      (loop (append (car al) rest) r comment skip-all? skipped)]
	     [else
	      (let ([skip? (or skip-all? (and condense? skip-ok? (eq? (car l) 'alts~)))])
		(let ([skipped (loop (car al) r comment skip? skipped)])
		  (aloop (cdr al) skipped)))])))]
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

  (define (slide/title/tall/inset/gap v-sep s inset . x)
    (apply do-slide/title/tall/inset do-add-slide! #t #t values v-sep s inset x))

  (define (slide/title/tall/inset s inset . x)
    (apply slide/title/tall/inset/gap gap-size s inset x))

  (define (slide/name/tall/inset s inset . x)
    (apply slide/title/tall/inset (make-name-only s) inset x))

  (define (slide/title/tall/gap v-sep s . x)
    (apply do-slide/title/tall/inset do-add-slide! #t #t values v-sep s zero-inset x))

  (define (slide/title/tall s . x)
    (apply slide/title/tall/gap gap-size s x))

  (define (slide/name/tall s . x)
    (apply slide/title/tall (make-name-only s) x))

  (define (slide/title s . x)
    (apply slide/title/tall/gap (* 2 gap-size) s x))

  (define (slide/name s . x)
    (apply slide/title (make-name-only s) x))

  (define (slide/title/inset s inset . x)
    (apply slide/title/tall/inset/gap (* 2 gap-size) s inset x))

  (define (slide/name/inset s inset . x)
    (apply slide/title/inset (make-name-only s) inset x))

  (define (slide/title/center/inset s inset . x)
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
	     #f
	     (lambda (x) (list (combine x)))
	     0 #f inset x)
      (apply do-slide/title/tall/inset
	     do-add-slide!
	     #t
	     #t
	     (lambda (x)
	       (list
		(cc-superimpose
		 (apply-slide-inset inset (if (string? s)
					      titleless-page 
					      full-page))
		 (ct-superimpose
		  (blank max-width max-height)
		  (combine x)))))
	     0 s inset x)))

  (define (slide/name/center/inset s inset . x)
    (apply slide/title/center/inset (make-name-only s) inset x))

  (define (slide/title/center s . x)
    (apply slide/title/center/inset s zero-inset x))

  (define (slide/name/center s . x)
    (apply slide/title/center (make-name-only s) x))

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

  (define (start-at-recent-slide)
    (set! current-page (max 0 (- page-number 2))))

  (define (make-outline . l)
    (define ah (arrowhead gap-size 0))
    (define current-item (colorize (hc-append (- (/ gap-size 2)) ah ah) blue))
    (define other-item (rc-superimpose (ghost current-item) (colorize ah "light gray")))
    (lambda (which)
      (slide/name
       (format "--~a--"
	       (let loop ([l l])
		 (cond
		  [(null? l) "<unknown>"]
		  [(eq? (car l) which)
		   (cadr l)]
		  [else (loop (cdddr l))])))
       (blank (+ title-h gap-size))
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
	(let* ([sep? (and (string? s) (regexp-match "^[',. :;-?!)]" s))]
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
    (if (not (and (= use-screen-w screen-w)
		  (= use-screen-h screen-h)))
	(scale p 
	       (/ screen-w use-screen-w)
	       (/ screen-h use-screen-h))
	p))

  ;; ----------------------------------------

  (define-struct click-region (left top right bottom thunk show-click?))

  (define (shift-click-region cr dx dy)
    (make-click-region (+ dx (click-region-left cr))
		       (+ dy (click-region-top cr))
		       (+ dx (click-region-right cr))
		       (+ dy (click-region-bottom cr))
		       (click-region-thunk cr)
		       (click-region-show-click? cr)))

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

  (define scroll-transition
    (opt-lambda (x y w h dx dy [duration 0.20] [steps 12])
      (add-transition! 'scroll-transition
		       (lambda (offscreen-dc)
			 (let* ([steps-done 0]
				[xs (/ use-screen-w screen-w)]
				[ys (/ use-screen-h screen-h)]
				[bcw (send (send offscreen-dc get-bitmap) get-width)]
				[bch (send (send offscreen-dc get-bitmap) get-height)]
				[mx (- margin (/ (- use-screen-w bcw) 2 xs))]
				[my (- margin (/ (- use-screen-h bch) 2 ys))]
				[x-space (ceiling (* xs (/ (abs dx) steps)))]
				[y-space (ceiling (* ys (/ (abs dy) steps)))]
				[x-in (if (positive? dx)
					  x-space
					  0)]
				[y-in (if (positive? dy)
					  y-space
					  0)])
			   (unless (and scroll-bm
					(>= (send scroll-bm get-width) 
					    (+ x-space (* xs w)))
					(>= (send scroll-bm get-height) 
					    (+ y-space (* ys h))))
			     (set! scroll-bm (make-bitmap
					      (inexact->exact (ceiling (+ x-space (* xs w))))
					      (inexact->exact (ceiling (+ y-space (* ys h))))))
			     (if (send scroll-bm ok?)
				 (send scroll-dc set-bitmap scroll-bm)
				 (set! scroll-bm #f)))

			   (when scroll-bm
			     (send scroll-dc clear)
			     (send scroll-dc draw-bitmap-section (send offscreen-dc get-bitmap)
				   x-in y-in
				   (* (+ x mx) xs) (* (+ y my) ys)
				   (* w xs) (* h ys)))
			   
			   (lambda (canvas offscreen-dc)
			     (if (or (not scroll-bm) (= steps-done steps))
				 'done
				 (let*-values ([(cw ch) (send canvas get-client-size)])
				   (let ([xm (- margin (/ (- use-screen-w bcw) 2 xs))]
					 [ym (- margin (/ (- use-screen-h bch) 2 ys))])
				     (set! steps-done (add1 steps-done))
				     (let ([draw
					    (lambda (dc xm ym)
					      (send dc draw-bitmap-section
						    scroll-bm
						    (- (* (+ x xm (* dx (/ steps-done steps))) xs) x-in)
						    (- (* (+ y ym (* dy (/ steps-done steps))) ys) y-in)
						    0 0 
						    (ceiling (* xs (+ w (/ (abs dx) steps))))
						    (ceiling (* ys (+ h (/ (abs dy) steps))))))])
				       (draw (send canvas get-dc) xm ym)
				       (draw offscreen-dc mx my)))
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
		    [slide/title/center/inset slide/title/inset-contract]
		    [slide/name slide/title-contract]
		    [slide/name/tall slide/title-contract]
		    [slide/name/center slide/title-contract]
		    [slide/name/inset slide/title/inset-contract]
		    [slide/name/tall/inset slide/title/inset-contract]
		    [slide/name/center/inset slide/title/inset-contract])
  (provide most-recent-slide retract-most-recent-slide re-slide start-at-recent-slide
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
	   margin set-margin! client-w client-h
	   full-page titleless-page
	   printing? condense? skip-slides
	   set-use-background-frame!
	   enable-click-advance!
	   title-h set-title-h! current-slide-assembler
	   current-page-number-font current-page-number-color 
	   set-page-numbers-visible!
	   (all-from (lib "mrpict.ss" "texpict"))
	   (all-from (lib "utils.ss" "texpict"))
           start-making-slides done-making-slides
	   started-from-launcher)
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
			       (augment on-close)
			       (define (on-close) (inner (void) on-close) (exit))
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
				   (define-values (orig-ox orig-oy) (send dc get-origin))
                                   (define scale (min (/ (- (/ client-h 2) margin) client-h)
                                                      (/ (- (/ client-w 2) margin) client-w)))
				   (define (set-origin x y)
				     (send dc set-origin (+ orig-ox (* x orig-sx)) (+ orig-oy (* y orig-sy))))
                                   (send dc set-scale (* orig-sx scale) (* orig-sy scale))
                                   (set-origin x y)
                                   ((slide-drawer a) dc 0 0)
                                   (set-origin (+ x (/ client-w 2) margin) y)
                                   ((slide-drawer b) dc 0 0)
                                   (set-origin x (+ y (/ client-h 2) margin))
                                   ((slide-drawer c) dc 0 0)
                                   (set-origin (+ x (/ client-w 2) margin) (+ y (/ client-h 2) margin))
                                   ((slide-drawer d) dc 0 0)
                                   (send dc set-scale orig-sx orig-sy)
                                   (set-origin x y)
                                   (send dc draw-line (/ client-w 2) 0 (/ client-w 2) client-h)
                                   (send dc draw-line 0 (/ client-h 2) client-w (/ client-h 2))
                                   (send dc set-origin orig-ox orig-oy))
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
                         (set! current-page (sub1 (length talk-slide-list)))
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
	    (when print-slide-seconds?
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
                (let ([old (list-ref talk-slide-list current-page)])
                  (set! current-page (min (add1 current-page)
                                          (sub1 (length talk-slide-list))))
                  (when print-slide-seconds?
                    (let ([slide-end-seconds (current-seconds)])
                      (printf "Slide ~a: ~a seconds~n" current-page
                              (- slide-end-seconds slide-start-seconds))
                      (set! slide-start-seconds slide-end-seconds)))
                  (do-transitions (slide-transitions old) (send c get-offscreen)))))

          (super-new)))
      
      (define-values (screen-left-inset screen-top-inset)
	(if keep-titlebar?
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
			     [width (inexact->exact (floor actual-screen-w))]
			     [height (inexact->exact (floor actual-screen-h))]
			     [style '(no-caption no-resize-border hide-menu-bar)])))))

      (when background-f
	(send background-f enable #f)
	(send background-f show #t))

      (define f (new talk-frame%
                     [closeable? keep-titlebar?]
		     [close-bg? #t]
                     [label (if content
                                (format "~a: slideshow" (file-name-from-path content))
                                "Slideshow")]
                     [x (- screen-left-inset)] [y (- screen-top-inset)]
                     [width (inexact->exact (floor actual-screen-w))]
                     [height (inexact->exact (floor actual-screen-h))]
                     [style (if keep-titlebar?
                                null
                                '(no-caption no-resize-border hide-menu-bar))]))
      
      (define f-both (new talk-frame%
			  [closeable? #t]
			  [close-bg? #f]
			  [label "Slideshow Preview"]
			  [x 0] [y 0]
			  [width (inexact->exact (floor actual-screen-w))]
			  [height (inexact->exact (quotient (floor actual-screen-h) 2))]
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
		(max 1 (- (inexact->exact (floor actual-screen-w)) 
			  (inexact->exact (floor (* (+ (sinset-l sinset) (sinset-r sinset))
						    (/ actual-screen-w screen-w))))))
		(max 1 (- (inexact->exact (floor actual-screen-h)) 
			  (inexact->exact (floor (* (+ (sinset-t sinset) (sinset-b sinset))
						    (/ actual-screen-h screen-h)))))))
	  (send f move 
		(inexact->exact (- (floor (* (sinset-l sinset) (/ actual-screen-w screen-w))) screen-left-inset))
		(inexact->exact (- (floor (* (sinset-t sinset) (/ actual-screen-h screen-h))) screen-top-inset)))
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
        (if (= 1 (slide-page-count slide))
            (format "~a" (slide-page slide))
            (format "~a-~a" (slide-page slide) (+ (slide-page slide)
                                                  (slide-page-count slide)
                                                  -1))))
      
      (define (calc-progress)
        (if (and start-time talk-duration-minutes)
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
      
      (define c%
        (class canvas%
          (inherit get-dc get-client-size)
          
          (define clicking #f)
          (define clicking-hit? #f)
          
          (define/override (on-paint)
            (let ([dc (get-dc)])
	      (stop-transition/no-refresh)
              (cond
	       [use-offscreen?
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
          
	  (define/private (paint-prefetch dc)
	    (let-values ([(cw ch) (get-client-size)])
	      (paint-letterbox dc cw ch use-screen-w use-screen-h)
	      (let ([dx (floor (/ (- cw use-screen-w) 2))]
		    [dy (floor (/ (- ch use-screen-h) 2))])
		(send dc draw-bitmap prefetch-bitmap dx dy)
		(set! click-regions (map (lambda (cr)
					   (shift-click-region cr dx dy))
					 prefetched-click-regions)))))

	  (define/override (on-size w h)
	    (unless resizing-frame?
	      (redraw)))

          (define/public (redraw)
            (reset-display-inset! (slide-inset (list-ref talk-slide-list current-page)))
            (send commentary lock #f)
            (send commentary erase)
            (let ([s (list-ref talk-slide-list current-page)])
              (when (just-a-comment? (slide-comment s))
                (send commentary insert (just-a-comment-text (slide-comment s)))))
            (send commentary lock #t)
	    (set! click-regions null)
	    (set! clicking #f)
	    (stop-transition/no-refresh)
	    (cond
              [use-offscreen?
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
		 [(and use-prefetch? use-prefetch-in-preview?)
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
	     (paint-slide dc page 1 1 cw ch use-screen-w use-screen-h #t))]
          [(dc page extra-scale-x extra-scale-y cw ch usw ush to-main?)
           (let* ([slide (list-ref talk-slide-list page)]
		  [ins (slide-inset slide)]
		  [cw (if to-main?
			  (+ cw (sinset-l ins) (sinset-r ins))
			  cw)]
		  [ch (if to-main?
			  (+ ch (sinset-t ins) (sinset-b ins))
			  ch)]
		  [sx (/ usw screen-w)]
		  [sy (/ ush screen-h)]
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
	       ((slide-drawer slide) dc margin margin)
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
                     (= use-screen-w (send prefetch-bitmap get-width))
                     (= use-screen-h (send prefetch-bitmap get-height)))
          (send prefetch-dc set-bitmap #f)
          (set! prefetch-bitmap (make-bitmap use-screen-w use-screen-h))
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
	  (when (and use-prefetch-in-preview?
		     (send f-both is-shown?))
	    (send c-both paint-prefetched))))

      (define (schedule-slide-prefetch n delay-msec)
	(cancel-prefetch)
	(when (and use-prefetch?
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
                          (equal? last-title (or (slide-title (car slides))
						 "(untitled)")))
                     (loop (cdr slides) (+ n 1) last-title)]
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
      
      (let ([bm (make-object bitmap% (build-path (collection-path "slideshow") "slideshow.bmp"))]
            [mbm (make-object bitmap% (build-path (collection-path "slideshow") "mask.xbm"))])
        (when (send bm ok?)
          (send f set-icon bm (and (send mbm ok?) mbm) 'both)))
      
      (send f show #t)
      (when two-frames?
        (send f-both show #t))
      
      (when commentary?
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
      
      (when printing?
        (let ([ps-dc (dc-for-text-size)])
          (let loop ([start? #f][l (list-tail talk-slide-list current-page)][n current-page])
            (unless (null? l)
              (set! current-page n)
              (refresh-page)
              (when start?
                (send ps-dc start-page))
              (let ([slide (car l)])
		(let ([xs (/ use-screen-w screen-w)]
		      [ys (/ use-screen-h screen-h)])
		  (send ps-dc set-scale xs ys)
		  ((slide-drawer slide) ps-dc 
		   (+ margin (/ (- actual-screen-w use-screen-w) 2 xs))
		   (+ margin (/ (- actual-screen-h use-screen-h) 2 ys))))
		(when show-page-numbers?
		  (send ps-dc set-scale 1 1)
                  (let ([s (slide-page-string slide)])
                    (let-values ([(w h) (send ps-dc get-size)]
                                 [(sw sh sd sa) (send ps-dc get-text-extent s)]
                                 [(hm vm) (values margin margin)])
                      (send ps-dc draw-text s (- w hm sw) (- h vm sh))))))
              (send ps-dc end-page)
              (loop #t (cdr l) (add1 n))))
          (send ps-dc end-doc)
          (exit)))))

  ;; If bitmap creation fails, try an explicit GC.
  ;; (This loop should be built into MrEd, but it wasn't
  ;; at the time this code was written.)
  (define (make-bitmap w h)
    (let loop ([n 0])
      (let ([bm (make-object bitmap% w h)])
	(if (or (= n 4)
		(send bm ok?))
	    bm
	    (begin
	      (collect-garbage)
	      (loop (add1 n)))))))

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
  
  (define (load-content content)
    (send progress-window show #t)
    (start-making-slides)
    (dynamic-require (path->complete-path content) #f)
    (done-making-slides))

  (when content
    (load-content (string->path content)))

  (define (started-from-launcher)
    (unless done-once?
      ;; GUI front-end when no slides are provided
      (let ()
	(define f (new dialog%
		       [label "Slideshow"]))
	(define msg-panel (new vertical-panel%
			       [parent f]
			       [alignment '(left center)]))
	(define button-panel (new horizontal-pane%
				  [parent f]
				  [stretchable-height #f]))
	(let ([t (lambda strs
		   (map (lambda (s)
			  (new message%
			       [label s]
			       [parent msg-panel]))
			strs))])
	  (t "To run a slide presentation, either"
	     " - provide the path of the main file on the Slideshow comamnd line, "
	     " - develop the talk in DrScheme using the (lib \"run.ss\" \"slideshow\") language, or "
	     " - click the Load button below to select a slide module. "
	     " "
	     "Click the Tutorial button below for more information. "
	     " "))
	(new button% 
	     [label "Tutorial"]
	     [parent button-panel]
	     [callback (lambda (b e)
			 (send f show #f)
			 (load-content
			  (build-path (collection-path "slideshow")
				      "tutorial-show.ss")))])
	(new pane% [parent button-panel]) ; spacer
	(new button%
	     [label "Load..."]
	     [parent button-panel]
	     [style '(border)]
	     [callback (lambda (b e)
			 (let ([file (get-file)])
			   (when file
			     (let-values ([(base name dir?) (split-path file)])
			       (current-directory base))
			     (send f show #f)
			     (load-content file))))])
	(new button%
	     [label "Quit"]
	     [parent button-panel]
	     [callback (lambda (b e)
			 (send f show #f))])

	(send progress-window show #f)
	(send f center)
	(send f show #t)))))
