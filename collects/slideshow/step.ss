#cs
(module step (lib "slideshow.ss" "texpict")
  (require (lib "list.ss"))

  (provide with-steps with-steps~)
  
  (define-syntax (with-steps stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr)
       #'(do-with-steps #f (step-name ...) expr)]))

  (define-syntax (with-steps~ stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr)
       #'(do-with-steps #t (step-name ...) expr)]))

  (define-syntax (define-step stx)
    (syntax-case stx ()
      [(_ func id steps (arg ...) body)
       (syntax/loc stx
	 (begin
	   (define (func arg ...)
	     body)
	   (define-syntax (id istx)
	     (syntax-case istx ()
	       [(_ arg ...)
		(begin
		  (unless (ormap (lambda (i)
				   (module-identifier=? i #'arg))
				 (syntax->list (quote-syntax steps)))
		    (raise-syntax-error
		     #f
		     "unknown step name"
		     istx
		     #'arg))
		  ...
		  (syntax/loc istx (func (quote arg) ...)))]))))]))

  (define-syntax (do-with-steps stx)
    (syntax-case stx ()
      [(_ condensing (step-name ...) expr)
       (let ([capturing (lambda (s)
			  (datum->syntax-object #'expr s))])
	 (with-syntax ([after (capturing 'after)]
		       [vafter (capturing 'vafter)]
		       [between (capturing 'between)]
		       [vbetween (capturing 'vbetween)]
		       [between-excl (capturing 'between-excl)]
		       [vbetween-excl (capturing 'vbetween-excl)])
	   #'(let ([steps '(step-name ...)])
	       (map (lambda (step)
		      (define-step after/p after (step-name ...)
			(p)
			(memq step (or (memq p steps) null)))
		      (define-step vafter/p vafter (step-name ...)
			(p)
			(if (after/p p) (let ([vafter (lambda (x) x)]) vafter) ghost))
		      (define-step between/p between (step-name ...)
			(p1 p2)
			(and (after/p p1) (or (eq? step p2) (not (after/p p2)))))
		      (define-step vbetween/p vbetween(step-name ...)
			(p1 p2)
			(if (between/p p1 p2) (let ([vbetween (lambda (x) x)]) vbetween) ghost))
		      (define-step between-excl/p between-excl (step-name ...)
			(p1 p2)
			(and (after/p p1) (not (after/p p2))))
		      (define-step vbetween-excl/p vbetween-excl (step-name ...)
			(p1 p2)
			(if (between-excl/p p1 p2) (let ([vbetween-excl (lambda (x) x)]) vbetween-excl) ghost))
		      expr)
		    (if (and condensing condense?)
			(last-pair steps)
			(if condense?
			    (filter (lambda (id)
				      (not (regexp-match #rx"~$" (symbol->string id))))
				    steps)
			    steps))))))])))
