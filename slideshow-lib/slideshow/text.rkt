#lang racket/base

(require slideshow/base pict
         racket/contract)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-with-parameter name parameter)
  (define-syntax-rule (name value body (... ...))
    (parameterize ([parameter value]) body (... ...))))

(define-with-parameter with-size current-font-size)
(define-syntax-rule (with-scale scale expr)
  (with-size (inexact->exact (ceiling (* scale (current-font-size)))) expr))
(define-syntax-rule (define-scale name scale)
  (define-syntax-rule (name expr) (with-scale scale expr)))
(define-scale big 3/2)
(define-scale small 2/3)

(define-with-parameter with-font current-main-font)
(define-syntax-rule (with-style style expr)
  (with-font (cons style (current-main-font)) expr))
(define-syntax-rule (define-style name style)
  (define-syntax-rule (name expr) (with-style style expr)))
(define-style bold 'bold)
(define-style italic 'italic)
(define-style subscript 'subscript)
(define-style superscript 'superscript)
(define-style caps 'caps)

(provide with-size
         with-scale
         big
         small

         with-font
         with-style
         bold
         italic
         subscript
         superscript
         caps)

(define (blank-line)
  (blank 0 (current-font-size)))

(provide/contract
 [blank-line (-> pict?)])
