
(module utils racket/base
  (require mred
           mzlib/class
           racket/format)

  (provide define-accessor
           define/provide-struct
           seconds->hhmmss)

  (define-syntax define-accessor
    (syntax-rules ()
      [(_ margin get-margin)
       (define-syntax margin
	 (syntax-id-rules ()
	   [(margin arg) ((get-margin) arg...)]
	   [margin (get-margin)]))]))


  (define-syntax define/provide-struct
    (syntax-rules ()
      [(_ id flds flags ...)
       (begin
	 (define-struct id flds flags ...)
	 (provide (struct-out id)))]))


  (define (seconds->hhmmss s)
    (define-values (hours left) (quotient/remainder s (* 60 60)))
    (define-values (minutes seconds) (quotient/remainder left 60))
    (string-append
     (~r hours #:min-width 2 #:pad-string "0") ":"
     (~r minutes #:min-width 2 #:pad-string "0") ":"
     (~r seconds #:min-width 2 #:pad-string "0"))))
