#lang racket/base

(provide aspect?)

(define (aspect? v)
  (or (not v)
      (eq? v 'widescreen)
      (eq? v 'fullscreen)))
