#lang info

(define collection 'multi)

(define build-deps '("scheme-lib"
                     "draw-doc"
                     "gui-doc"
                     "pict-doc"
                     "scribble-doc"
                     "base"
                     "gui-lib"
                     "pict-lib"
                     "scribble-lib"
                     "slideshow-lib"
                     ["racket-doc" #:version "1.3"]
                     "at-exp-lib"))
(define update-implies '("slideshow-lib"))

(define pkg-desc "documentation part of \"slideshow\"")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
