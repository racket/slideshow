#lang racket/base
(require mred
         mzlib/class
         racket/format)

(provide define-accessor
         define/provide-struct
         adjust-keyword-default
         seconds->hhmmss)

(define-syntax define-accessor
  (syntax-rules ()
    [(_ margin get-margin extra-arg ...)
     (define-syntax margin
       (syntax-id-rules ()
         [(margin arg (... ...)) ((get-margin extra-arg ...) arg (... ...))]
         [margin (get-margin extra-arg ...)]))]))

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
   (~r seconds #:min-width 2 #:pad-string "0")))

(define (adjust-keyword-default proc kw kw-val)
  (define-values (req opt) (procedure-keywords proc))
  (procedure-reduce-keyword-arity
   (make-keyword-procedure
    (lambda (kws kw-vals . args)
      (if (memq kw kws)
          (keyword-apply proc kws kw-vals args)
          (keyword-apply-with-keyword proc kw kw-val kws kw-vals args))))
   (procedure-arity proc)
   req
   opt))

(define (keyword-apply-with-keyword proc kw kw-val kws kw-vals args)
  (let loop ([kws kws] [kw-vals kw-vals] [rev-kws '()] [rev-kw-vals '()])
    (cond
      [(or (null? kws)
           (keyword<? kw (car kws)))
       (define new-kws (append (reverse rev-kws) (list kw) kws))
       (define new-kw-vals (append (reverse rev-kw-vals) (list kw-val) kw-vals))
       (keyword-apply proc new-kws new-kw-vals args)]
      [else
       (loop (cdr kws) (cdr kw-vals) (cons (car kws) rev-kws) (cons (car kw-vals) rev-kw-vals))])))
