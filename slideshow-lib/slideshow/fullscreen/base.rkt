#lang racket/base
(require (for-syntax racket/base)
         "../base.rkt" ; definitions shadow imports to replace
         (prefix-in slide: "../slide.rkt")
         "../private/utils.rkt")

(define slide (adjust-keyword-default slide:slide/kw '#:aspect 'fullscreen))
(define make-outline (adjust-keyword-default slide:make-outline '#:aspect 'fullscreen))
(define item (adjust-keyword-default slide:item/kw '#:aspect 'fullscreen))
(define subitem (adjust-keyword-default slide:subitem/kw '#:aspect 'fullscreen))
(define para (adjust-keyword-default slide:para/kw '#:aspect 'fullscreen))
(define size-in-pixels (adjust-keyword-default slide:size-in-pixels '#:aspect 'fullscreen))

(define-accessor client-w slide:get-client-w #:aspect 'fullscreen)
(define-accessor client-h slide:get-client-h #:aspect 'fullscreen)
(define-accessor full-page slide:get-full-page #:aspect 'fullscreen)
(define-accessor titleless-page slide:get-titleless-page #:aspect 'fullscreen)

(define current-para-width (get-current-para-width #:aspect 'fullscreen))

(provide slide
         (all-from-out "../base.rkt")
         make-outline
         item
         subitem
         para
         client-w client-h
         full-page titleless-page
         size-in-pixels
         current-para-width)
