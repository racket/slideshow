(module run "slideshow.ss"
  (provide (all-from-except "slideshow.ss" #%module-begin)
           (rename slideshow-module-begin #%module-begin))
  
  (define-syntax (slideshow-module-begin stx)
    (syntax-case stx ()
      [(_ e ...)
       (datum->syntax-object
	stx
	(syntax-e
	 (syntax (#%module-begin
		  (start-making-slides)
		  e ...
		  (done-making-slides)))))])))

