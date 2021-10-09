#lang info

(define collection 'multi)

(define deps '("slideshow-lib" 
               "slideshow-exe"
               "slideshow-plugin"
               "slideshow-doc"))
(define implies '("slideshow-lib" 
                  "slideshow-exe"
                  "slideshow-plugin"
                  "slideshow-doc"))

(define pkg-desc "Slide-presentation tool")

(define pkg-authors '(mflatt robby))

(define license
  '(Apache-2.0 OR MIT))
