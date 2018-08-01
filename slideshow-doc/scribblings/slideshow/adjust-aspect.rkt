#lang at-exp racket/base
(require scribble/manual
         (for-syntax racket/base))

(provide declare-aspect-adjust
         define-aspect-select)

(define-syntax-rule (declare-aspect-adjust proc def)
  (begin
    (define-syntax-rule (define-it from-base)
      (begin
        (require (for-label (only-in slideshow/base [proc proc])))
        (define from-base @racket[proc])))
    (define-it from-base)
    @defthing[proc procedure?]{
         The same as @from-base frmo @racketmodname[slideshow/base], but with @racket[def] as the default
         value of the @racket[#:aspect] argument.}))

(define-syntax-rule (define-aspect-select id get-id def)
  @defidform[id]{The same as @racket[(get-id def)].})
