#lang racket
(require "widescreen/base.rkt"
         "pict.rkt")
(provide (except-out (all-from-out racket
                                   "widescreen/base.rkt"
                                   "pict.rkt")
                     printable<%>))

(module reader syntax/module-reader
  slideshow/widescreen)
