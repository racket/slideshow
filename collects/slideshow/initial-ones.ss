
(module initial-ones (lib "slideshow.ss" "slideshow")
  (require (lib "code.ss" "slideshow")
	   (lib "etc.ss")
	   (lib "package.ss"))

  (provide do-initial-slides)

  (define (do-initial-slides)
    ;; We use package just to support a top-level-like sequence of
    ;; definitions and expressions
    (package p ()
      (slide/center
       (titlet "Slideshow Tutorial")
       (blank)
       (bitmap (build-path (collection-path "icons") "PLTnolarval.jpg"))
       (blank)
       (colorize (it "Press the spacebar to continue") blue))

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

      (slide/title/center
       "How to Control this Viewer"
       (table 3
	      (apply
	       append
	       (map (lambda (s)
		      (list (apply page-para* (car s)) (t ":") (t (cadr s))))
		    `(((,(hb-append (t "Alt-") (tt "q")) ,(hbl-append (t "or Meta-") (tt "q"))) "ends show")
		      ((,sym:rightarrow ", Space," ,(tt "f") ", or" ,(tt "n")) "next slide")
		      ((,sym:leftarrow "or" ,(tt "b")) "previous slide")
		      ((,(tt "g")) "last slide")
		      ((,(tt "1")) "first slide")
		      ((,(hbl-append (t "Alt-") (tt "g") (t " or Meta-") (tt "g"))) "select a slide"))))
	      lbl-superimpose lbl-superimpose
	      gap-size (/ gap-size 2)))

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
       (sub-para "registers a slide containing only" (colorize (t "Hello") "black"))
       (blank)
       (with-font 
	`(italic . ,main-font)
	(lambda ()
	  (para (* 3/4 client-w)
		"Technically, the pict concept comes from the"
		(tt "\"texpict\"") "collection. The distinction between"
		"Slideshow and texpict matters when you"
		"use Help Desk to find information on other pict functions."
		"For now, we ignore the distinction."))))
      
      (slide/title/center
       "The Rest of the Tutorial"
       (page-para "The rest of this tutorial (starting with the next slide) is meant to"
		  "be viewed while reading the program source")
       (blank)
       (page-para "The source is")
       (let ([s (build-path (this-expression-source-directory)
			    "tutorial-show.ss")])
	 (scale/improve-new-text
	  (tt s)
	  (min 1 (/ (* 0.8 client-w ) (pict-width (tt s)))))))
      
      
      )
    (void)))

