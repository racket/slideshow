#cs
(module step (lib "slideshow.ss" "slideshow")
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

  (define-syntax (define-predicate/vproc stx)
    (syntax-case stx ()
      [(_ pred pred/p vproc steps (arg ...) body)
       #'(begin
	   (define-step pred/p pred steps (arg ...) body)
	   (define-step v vproc steps (arg ...)
	     (if (pred/p arg ...) 
		 (let ([vproc (lambda (x) x)]) vproc)
		 (let ([vproc (lambda (x) (ghost x))]) vproc))))]))
       
  (define-syntax (do-with-steps stx)
    (syntax-case stx ()
      [(_ condensing (step-name ...) expr)
       (let ([capturing (lambda (s)
			  (datum->syntax-object #'expr s))])
	 (with-syntax ([only? (capturing 'only?)]
		       [vonly (capturing 'vonly)]
		       [before? (capturing 'before?)]
		       [vbefore (capturing 'vbefore)]
		       [after? (capturing 'after?)]
		       [vafter (capturing 'vafter)]
		       [between? (capturing 'between?)]
		       [vbetween (capturing 'vbetween)]
		       [between-excl? (capturing 'between-excl?)]
		       [vbetween-excl (capturing 'vbetween-excl)])
	   #'(let ([steps '(step-name ...)])
	       (map (lambda (step)
		      (define-predicate/vproc only? only?/p vonly (step-name ...)
			(p)
			(eq? step p))
		      (define-predicate/vproc after? after?/p vafter (step-name ...)
			(p)
			(memq step (or (memq p steps) null)))
		      (define-predicate/vproc before? vbefore?/p vbefore (step-name ...)
			(p)
			(not (after?/p p)))
		      (define-predicate/vproc between? between?/p vbetween (step-name ...)
			(p1 p2)
			(and (after?/p p1) (or (eq? step p2) (not (after?/p p2)))))
		      (define-predicate/vproc between-excl? between-excl?/p vbetween-excl (step-name ...)
			(p1 p2)
			(and (after?/p p1) (not (after?/p p2))))
		      expr)
		    (if (and condensing condense?)
			(last-pair steps)
			(if condense?
			    (filter (lambda (id)
				      (not (regexp-match #rx"~$" (symbol->string id))))
				    steps)
			    steps))))))])))
