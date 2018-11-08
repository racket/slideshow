#lang racket/base

(require slideshow/slide
         (only-in racket/list last-pair)
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(provide with-steps with-steps~)

(define-syntax-parser define-step
  [(_ func:id macro:id steps-ids:id (arg:id ...)
      (~optional
       (~seq #:extra-args (reqd-extra-arg:id ...
                           [opt-extra-arg:id
                            opt-arg-default:expr] ...)))
      body:expr)
   (syntax/loc this-syntax
     (begin
       (define (func arg ...
                     (~? (~@ reqd-extra-arg ...))
                     (~? (~@ [opt-extra-arg opt-arg-default] ...)))
         body)
       (define-syntax-parser macro
         #:disable-colon-notation
         [(_ (~var arg id) ...
             (~? (~@ (~var reqd-extra-arg expr) ...))
             (~? (~@ (~optional (~var opt-extra-arg expr)) ...)))
          #:do [(define steps-ids* (syntax-local-value #'steps-ids))]
          #:fail-when (for/first ([id (in-list (list #'arg ...))]
                                  #:unless
                                  (member id steps-ids* free-identifier=?))
                        id)
          "unknown step name"
          (with-disappeared-uses
           (record-disappeared-uses (list #'arg) ...)
           (syntax/loc this-syntax
             (func (quote arg) ...
                   (~? (~@ reqd-extra-arg ...))
                   (~? (~@ ((... ~?) opt-extra-arg) ...)))))])))])


(define-syntax-parser define-predicate/vproc
  [(_ pred:id pred/p:id vproc:id proc:id
      steps-ids:id
      (arg:id ...)
      body:expr)
   #'(begin
       (define-step pred/p pred steps-ids (arg ...) body)
       (define-step v proc steps-ids (arg ...)
         #:extra-args (f [else-f values])
         (if (pred/p arg ...) 
             f
             else-f))
       (define-step v2 vproc steps-ids (arg ...)
         (if (pred/p arg ...) 
             (let ([vproc (lambda (x) x)]) vproc)
             (let ([vproc (lambda (x) (ghost x))]) vproc))))])


(define-for-syntax (make-with-steps #:with-steps~ [for-with-steps~? #f])
  (syntax-parser
    [(_ (step-name:id ...) body0:expr body:expr ...)
     #:do [(define step-names (syntax->list #'(step-name ...)))]
     #:with (condense?-body ...)
     (if for-with-steps~?
         #`((skip-slides '#,(sub1 (length step-names)))
            (last-pair steps))
         (let ([non~-step-names
                (filter (λ (id) 
                          (not (regexp-match? #rx"~$" (symbol->string
                                                       (syntax->datum id)))))
                        step-names)])
           #`[(skip-slides '#,(- (length step-names) (length non~-step-names)))
              '(#,@non~-step-names)]))
     (define-syntax-rule (with-captured
                          (name ...)
                          #:context context-expr
                          body ...)
       (let ([context context-expr])
         (with-syntax ([name (datum->syntax context 'name)] ...)
           body ...)))
     (syntax-property
      (with-captured
       (only? vonly only except? vexcept except
              before? vbefore before after? vafter after
              between? vbetween between between-excl? vbetween-excl between-excl)
       #:context #'body0
       #`(let ([steps '(step-name ...)])
           (map (lambda (step)
                  (define-syntax steps-ids
                    (list (quote-syntax step-name) ...))
                  (define-predicate/vproc only? only?/p vonly only steps-ids
                    (p)
                    (eq? step p))
                  (define-predicate/vproc except? except?/p vexcept except steps-ids
                    (p)
                    (not (eq? step p)))
                  (define-predicate/vproc after? after?/p vafter after steps-ids
                    (p)
                    (memq step (or (memq p steps) null)))
                  (define-predicate/vproc before? vbefore?/p vbefore before steps-ids
                    (p)
                    (not (after?/p p)))
                  (define-predicate/vproc between? between?/p vbetween between steps-ids
                    (p1 p2)
                    (and (after?/p p1) (or (eq? step p2) (not (after?/p p2)))))
                  (define-predicate/vproc between-excl? between-excl?/p vbetween-excl between-excl steps-ids
                    (p1 p2)
                    (and (after?/p p1) (not (after?/p p2))))
                  (let () body0 body ...))
                (cond
                  [condense?
                   condense?-body ...]
                  [else
                   steps]))))
      'disappeared-binding (map syntax-local-introduce step-names))]))


  
(define-syntax with-steps
  (make-with-steps))

(define-syntax with-steps~
  (make-with-steps #:with-steps~ #t))

