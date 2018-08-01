#lang racket
(require "fullscreen/base.rkt"
         "pict.rkt")
(provide (except-out (all-from-out racket
                                   "fullscreen/base.rkt"
                                   "pict.rkt")
                     printable<%>))

(module reader syntax/module-reader
  slideshow/fullscreen)
