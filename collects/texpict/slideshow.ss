;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;               Command Line                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-values (screen-w screen-h) (values 1024 768))

(define printing? #f)
(define commentary? #f)
(define show-gauge? #f)

(define current-page 0)

(begin-elaboration-time
 (require-library "cmdline.ss"))
(require-library "cmdline.ss")

(define content
  (command-line
   "talk"
   argv
   [once-each
    (("--print") "print"
		 (set! printing? #t))
    (("-p") page "set the starting page"
	    (let ([n (string->number page)])
	      (unless (and n (integer? n)
			   (positive? n))
		(error 'talk "argument to -n is not a positive integer: ~a" page))
	      (set! current-page (sub1 n))))
    (("-c") "display commentary"
	    (set! commentary? #t))]
   [args (lecture-file)
	 lecture-file]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Setup                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define font-size 28)
(define line-sep 2)
(define title-size (+ font-size 4))
(define main-font 'swiss)

(define red "red")
(define green "medium sea green")
(define blue "blue")
(define purple "purple")

(require-library "mrpict.ss" "texpict")
(require-library "utils.ss" "texpict")

(define (t s) (text s main-font font-size))
(define (it s) (text s `(italic . ,main-font) font-size))
(define (bt s) (text s `(bold . ,main-font) font-size))
(define (bit s) (text s `(bold italic . ,main-font) font-size))
(define (tt s) (text s '(bold . modern) font-size))
(define (titlet s) (colorize (text s 
				   `(bold . ,main-font) 
				   title-size)
			     green))

(define bullet (cc-superimpose (disk (/ font-size 2)) 
			       (blank 0 font-size)))

(dc-for-text-size 
 (if printing?
     ;; Make a dummy ps file
     (let ([pss (make-object ps-setup%)])
       (parameterize ([current-ps-setup pss])
	 (send pss set-mode 'file)
	 (send pss set-file "TMP.ps")
	 (let ([p (make-object post-script-dc% #f)])
	   (send p start-doc "tmp")
	   (send p start-page)
	   (set-values! (screen-h screen-w) (send p get-size))
	   p)))

     ;; Bitmaps give same size as the screen:
     (make-object bitmap-dc% (make-object bitmap% 1 1))))

(define margin 10)
(define full-page (blank (- screen-w (* margin 2))
			 (- screen-h (* margin 2))))

(define talk-slide-list null)
(define-struct slide (drawer title comment))
(define-struct comment (text))

(define (add-slide! pict title comment)
  (set! talk-slide-list (cons
			 (make-slide (make-pict-drawer pict)
				     title 
				     comment)
			 talk-slide-list)))

(define (slide/title s . x) 
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
      (apply vc-append font-size
	     (map
	      (lambda (p)
		(let ([w (pict-width p)])
		  ;; Force even size:
		  (inset p 0 0 (+ (- w (floor w)) (modulo (floor w) 2)) 0)))
	      (if s
		  (cons (titlet s) x)
		  x))))
     s
     comment)))

(define (slide . x) (apply slide/title #f x))

(define (slide/title/stages s . x)
  (let loop ([l x][r null][comment #f])
    (cond
     [(null? l) (apply slide/title s (reverse r))]
     [(memq (car l) '(NEXT NEXT!))
      (unless (and printing? (eq? (car l) 'NEXT))
	(apply slide/title s (reverse r)))
      (loop (cdr l) r comment)]
     [(memq (car l) '(ALTS ALTS~)) 
      (let ([rest (cddr l)])
	(let aloop ([al (cadr l)])
	  (if (null? (cdr al))
	      (loop (append (car al) rest) r comment)
	      (begin
		(unless (and printing? (eq? (car l) 'ALTS~))
		  (loop (car al) r comment))
		(aloop (cdr al))))))]
     [else (loop (cdr l) (cons (car l) r) comment)])))

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

(define (para s w)
  (let loop ([s s][r ""])
    (let ([p (t s)])
      (let ([m (if (< (pict-width p) w)
		   #f
		   (regexp-match "(.*) (.*)" s))])
	(if m
	    (loop (cadr m) (string-append
			    (caddr m)
			    (if (string=? r "") "" " ")
			    r))
	    (if (string=? r "")
		p
		(vl-append
		 line-sep
		 p
		 (loop r ""))))))))

(define (item s)
  (htl-append (/ font-size 2)
	      bullet 
	      (para s (- screen-w 
			 (pict-width bullet) 
			 (/ font-size 2)))))

(define (itemize . l)
  (apply
   vl-append
   (let loop ([l l])
     (cond
      [(null? l) null]
      [else (cons (item (car l))
		  (loop (cdr l)))]))))

(define (page-item s)
  (cc-superimpose (item s) (blank screen-w 0)))

(define (page-itemize . l)
  (cc-superimpose (apply itemize l) (blank screen-w 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Talk                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load content)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                Talk Viewer                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set! talk-slide-list (reverse talk-slide-list))

(define TALK-MINUTES 25)
(define GAUGE-WIDTH 100)
(define GAUGE-HEIGHT 4)

(define talk-frame%
  (class frame% (closeable? . args)
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
      (apply super-init args))))

(define f (make-object talk-frame% #f "Talk" #f screen-w screen-h 0 0 '(no-caption)))

(define c-frame (make-object talk-frame% #t "Commentary" #f 400 100))
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

(define c% (class canvas% args
	     (inherit get-dc get-client-size)
	     (private
	       [number-font (make-object font% 10 'default 'normal 'normal)])
	     (override
	       [on-paint
		(lambda ()
		  (let* ([dc (get-dc)]
			 [f (send dc get-font)]
			 [c (send dc get-text-foreground)]
			 [s (format "~a" (add1 current-page))])
		    (let*-values ([(cw ch) (get-client-size)]
				  [(m) (- margin (/ (- screen-w cw) 2))])
		      ((slide-drawer (list-ref talk-slide-list current-page)) 
		       (get-dc) m m))
		    
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
		      (send dc draw-text s (- cw w 10) (- ch h 10)) ; 5+5 border
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
	 [l (make-object list-box% #f (let loop ([pages talk-slide-list][n 1])
					(if (null? pages)
					    null
					    (cons (format "~a. ~a" 
							  n 
							  (or (slide-title (car pages))
							      "(untitled)"))
						  (loop (cdr pages) (add1 n)))))
			 d void)]
	 [p (make-object horizontal-pane% d)])
    (send d center)
    (send p set-alignment 'right 'center)
    (send p stretchable-height #f)
    (make-object button% "Cancel" p (lambda (b e) (send d show #f)))
    (make-object button% "Ok" p 
		 (lambda (b e)
		   (send d show #f)
		   (let ([i (send l get-selection)])
		     (when i
		       (set! current-page i)
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
  (unless (directory-exists? "ps")
    (make-directory "ps"))
  (send (current-ps-setup) set-orientation 'landscape)
  (send (current-ps-setup) set-scaling 0.8 0.8)
  (send (current-ps-setup) set-mode 'file)
  (send (current-ps-setup) set-file "talk.ps")
  (let ([ps-dc (make-object post-script-dc%)])
    (send ps-dc start-doc "Talk")
    (let loop ([l (list-tail talk-slide-list current-page)][n current-page])
      (unless (null? l)
	(set! current-page n)
	(refresh-page)
	(send ps-dc start-page)
	((slide-drawer (car l)) ps-dc 0 0)
	(send ps-dc end-page)
	(loop (cdr l) (add1 n))))
    (send ps-dc end-doc)))
