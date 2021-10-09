#lang info

(define collection 'multi)

(define deps '("base"
               "compatibility-lib"
               "gui-lib"
               "pict-lib"
               "slideshow-lib"))
(define implies '("slideshow-lib"))

(define pkg-desc "executables for \"slideshow\"")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
