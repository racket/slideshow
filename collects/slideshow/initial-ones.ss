
;; Note that this module uses the "slideshow.ss" language
;;   instead of the "run.ss" language
;; Only the main module should use "run.ss"
(module initial-ones (lib "slideshow.ss" "slideshow")
  (require (lib "code.ss" "slideshow")
	   (lib "package.ss")
	   (lib "mred.ss" "mred")
	   (lib "class.ss"))

  (provide do-initial-slides)

  (define (do-initial-slides)
    ;; We use package just to support a top-level-like sequence of
    ;; definitions and expressions
    (package p ()
      (slide/name/center
       "Title Slide"
       (titlet "Slideshow Tutorial")
       (blank)
       (size-in-pixels
	(bitmap (build-path (collection-path "icons") "PLTnolarval.jpg")))
       (blank)
       (colorize (it "Press the spacebar to continue") blue)
       (comment "Welcome to Slideshow"))

      (slide/title/center
       "About Slideshow"
       (page-para (bt "Slideshow")
		  "is a library for creating slide presentations")
       (page-item "A Slideshow presentation is a PLT Scheme program")
       (page-item "Instead of a WYSIWYG interface,"
		  "you get the power of Scheme"))
      
      (define (symbol n)
	(text (string (integer->char n)) 'symbol font-size))
      (define sym:rightarrow (symbol 174))
      (define sym:leftarrow (symbol 172))

      (define (meta key)
	(hbl-append (t "Alt-") 
		    (if (pict? key) key (tt key))
		    (t ", Cmd-") 
		    (if (pict? key) key (tt key))
		    (t ", or Meta-") 
		    (if (pict? key) key (tt key))))

      (slide/title
       "How to Control this Viewer"
       (table 3
	      (apply
	       append
	       (map (lambda (s)
		      (list (apply page-para* (car s)) (t ":") (t (cadr s))))
		    `(((,(meta "q")) "end show")
		      (("Esc") "if confirmed, end show")
		      ((,sym:rightarrow ", Space," ,(tt "f") "," ,(tt "n") ", or click") "next slide")
		      ((,sym:leftarrow "or" ,(tt "b")) "previous slide")
		      ((,(tt "g")) "last slide")
		      ((,(tt "1")) "first slide")
		      ((,(meta "g")) "select a slide")
		      ((,(meta "p")) "show/hide slide number")
		      ((,(meta "c")) "show/hide commentary")
		      ((,(meta "d")) "show/hide preview")
		      ((,(hbl-append (t "Shift-") sym:rightarrow) ", etc.") "move window 1 pixel")
		      ((,(meta sym:rightarrow) ", etc.") "move window 10 pixels"))))
	      lbl-superimpose lbl-superimpose
	      gap-size (/ gap-size 2))
       (comment "This window shows comments for each slide. "
		"The comments are typically fill in the details of what "
		"the slide presenter says when giving the talk."))
		

      (define mytalk.scm (tt "mytalk.scm"))

      (slide/title
       "Slideshow Programs"
       (page-para "A Slideshow program has the form")
       (code (module mytalk (lib "run.ss" "slideshow")
	       ... #,(it "code to generate slide content") ...))
       (page-para "in a file named" mytalk.scm)
       (colorize (hline (* 3/4 client-w) gap-size) "green")
       'alts
       (list (list (page-para "Run a Slideshow program in DrScheme as follows:")
		   (page-item "Open" mytalk.scm "in DrScheme")
		   (page-item "Select" (bt "Choose Langauge") "from the" (bt "Language") "menu")
		   (page-item "Choose the" (tt "(module ...)") "language")
		   (page-item "Click" (bt "Execute")))
	     (list (page-para "You can also execute it from the command line:")
		   (hbl-append (tt "slideshow ") mytalk.scm)
		   (blank)
		   (page-para "To print the talk:")
		   (hbl-append (tt "slideshow --print ") mytalk.scm)
		   (blank)
		   (colorize
		    (page-para/r (it "Run") (tt "slideshow --help") (it "for more options"))
		    "blue"))))

      (define (sub-para . l)
	(colorize (apply para (* 3/4 client-w) l) "blue"))

      (slide/title
       "Slides and Picts"
       (page-para "The body of a Slideshow program")
       (page-item/bullet (bt " 1.")
			 "Makes and combines" (hbl-append (bit "pict") (t "s")))
       (sub-para "For example,")
       (code (t "Hello"))
       (sub-para "creates a pict like this:")
       (colorize (t "Hello") "black")
       (page-item/bullet (bt " 2.") "Registers certain picts as slides")
       (sub-para "For example,")
       (code (slide (t "Hello")))
       (sub-para "registers a slide containing only" (colorize (t "Hello") "black")))

      (slide/title/center
       "Slides versus Picts"
       (page-para "Technically, the pict concept comes from the"
		  (tt "\"texpict\"") "collection, and the"
		  (tt "\"slideshow\"") "collection builds on it")
       (page-item "The distinction between"
		  "Slideshow and texpict matters when you"
		  "use Help Desk to find information")
       (page-item "For now, we ignore the distinction"))

      (slide/title/center
       "The Rest of the Tutorial"
       (page-para "The rest of this tutorial (starting with the next slide) is meant to"
		  "be viewed while reading the program source")
       (blank)
       (page-para "The source is")
       (let ([s (build-path (collection-path "slideshow") "tutorial-show.ss")])
	 (clickback
	  (scale/improve-new-text
	   (let ([p (tt s)])
	     (colorize
	      (place-over p 0 (pict-height p)
			  (linewidth 2 (hline (pict-width p) 2)))
	      "blue"))
	   (min 1 (/ (* 0.8 client-w ) (pict-width (tt s)))))
	  (lambda ()
	    (let* ([f (new frame% 
			   [label "tutorial-show.ss"]
			   [width 600]
			   [height 400])]
		   [e (new text%)]
		   [c (new editor-canvas% 
			   [parent f]
			   [editor e])])
	      (send e load-file s)
	      (send e change-style 
		    (make-object style-delta% 'change-family 'modern)
		    0 'end)
	      (send f show #t))))))
      )
    (void)))

