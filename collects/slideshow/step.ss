#cs
(module step (lib "slideshow.ss" "slideshow")
  (require (lib "list.ss")
	   (lib "etc.ss"))

  (provide with-steps with-steps~)
  
  (define-syntax (with-steps stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr0 expr ...)
       #'(do-with-steps #f (step-name ...) expr0 expr ...)]))

  (define-syntax (with-steps~ stx)
    (syntax-case stx ()
      [(_ (step-name ...) expr0 expr ...)
       #'(do-with-steps #t (step-name ...) expr0 expr ...)]))

  (define-syntax (define-step stx)
    (syntax-case stx ()
      [(_ func id steps (arg ...) extra-args body)
       (syntax/loc stx
	 (begin
	   (define (func arg ... . extra-args)
	     body)
	   (define-syntax (id istx)
	     (syntax-case istx ()
	       [(_ arg ... . extra-args)
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
		  (syntax/loc istx (func (quote arg) ... . extra-args)))]))))]))

  (define-syntax (define-predicate/vproc stx)
    (syntax-case stx ()
      [(_ pred pred/p vproc proc steps (arg ...) body)
       #'(begin
	   (define-step pred/p pred steps (arg ...) () body)
	   (define-step v vproc steps (arg ...) ()
	     (if (pred/p arg ...) 
		 (let ([vproc (lambda (x) x)]) vproc)
		 (let ([vproc (lambda (x) (ghost x))]) vproc)))
	   (define-step v2 proc steps (arg ...) xform
	     (if (pred/p arg ...) 
		 (let ([vproc (apply compose xform)]) vproc)
		 (let ([vproc (lambda (x) x)]) vproc))))]))
       
  (define-syntax (do-with-steps stx)
    (syntax-case stx ()
      [(_ condensing (step-name ...) expr0 expr ...)
       (let ([capturing (lambda (s)
			  (datum->syntax-object #'expr0 s))])
	 (with-syntax ([only? (capturing 'only?)]
		       [vonly (capturing 'vonly)]
		       [only (capturing 'only)]
		       [except? (capturing 'except?)]
		       [vexcept (capturing 'vexcept)]
		       [except (capturing 'except)]
		       [before? (capturing 'before?)]
		       [vbefore (capturing 'vbefore)]
		       [before (capturing 'before)]
		       [after? (capturing 'after?)]
		       [vafter (capturing 'vafter)]
		       [after (capturing 'after)]
		       [between? (capturing 'between?)]
		       [vbetween (capturing 'vbetween)]
		       [between (capturing 'between)]
		       [between-excl? (capturing 'between-excl?)]
		       [vbetween-excl (capturing 'vbetween-excl)]
		       [between-excl (capturing 'between-excl)])
	   #'(let ([steps '(step-name ...)])
	       (map (lambda (step)
		      (define-predicate/vproc only? only?/p vonly only (step-name ...)
			(p)
			(eq? step p))
		      (define-predicate/vproc except? except?/p vexcept except (step-name ...)
			(p)
			(not (eq? step p)))
		      (define-predicate/vproc after? after?/p vafter after (step-name ...)
			(p)
			(memq step (or (memq p steps) null)))
		      (define-predicate/vproc before? vbefore?/p vbefore before (step-name ...)
			(p)
			(not (after?/p p)))
		      (define-predicate/vproc between? between?/p vbetween between (step-name ...)
			(p1 p2)
			(and (after?/p p1) (or (eq? step p2) (not (after?/p p2)))))
		      (define-predicate/vproc between-excl? between-excl?/p vbetween-excl between-excl (step-name ...)
			(p1 p2)
			(and (after?/p p1) (not (after?/p p2))))
		      (let () expr0 expr ...))
		    (if (and condensing condense?)
			(last-pair steps)
			(if condense?
			    (filter (lambda (id)
				      (not (regexp-match #rx"~$" (symbol->string id))))
				    steps)
			    steps))))))])))
