#lang racket/base
(require (for-syntax racket/base)
         "../base.rkt" ; definitions shadow imports to replace
         (prefix-in slide: "../slide.rkt")
         "../private/utils.rkt")

(define slide (adjust-keyword-default slide:slide/kw '#:aspect 'widescreen))
(define make-outline (adjust-keyword-default slide:make-outline '#:aspect 'widescreen))
(define item (adjust-keyword-default slide:item/kw '#:aspect 'widescreen))
(define subitem (adjust-keyword-default slide:subitem/kw '#:aspect 'widescreen))
(define para (adjust-keyword-default slide:para/kw '#:aspect 'widescreen))
(define size-in-pixels (adjust-keyword-default slide:size-in-pixels '#:aspect 'widescreen))

(define-accessor client-w slide:get-client-w #:aspect 'widescreen)
(define-accessor client-h slide:get-client-h #:aspect 'widescreen)
(define-accessor full-page slide:get-full-page #:aspect 'widescreen)
(define-accessor titleless-page slide:get-titleless-page #:aspect 'widescreen)

(define current-para-width (get-current-para-width #:aspect 'widescreen))

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
