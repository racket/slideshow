#lang scribble/manual
@(require (for-label slideshow/fullscreen/base)
          "adjust-aspect.rkt")

@title[#:tag "fullscreen"]{Fullscreen Slides}

@declare-exporting[slideshow/fullscreen/base slideshow/fullscreen]

@defmodulelang*/no-declare[(slideshow/fullscreen)]
@defmodule*/no-declare[(slideshow/fullscreen/base)]

The @racketmodname[slideshow/fullscreen/base] module is reprovided by
the @racket[slideshow/fullscreen] language along with
@racketmodname[racket] and @racketmodname[pict].

@history[#:added "1.5"]

@(declare-aspect-adjust slide 'fullscreen)
@(declare-aspect-adjust para 'fullscreen)
@(declare-aspect-adjust item 'fullscreen)
@(declare-aspect-adjust subitem 'fullscreen)
@(declare-aspect-adjust make-outline 'fullscreen)
@(declare-aspect-adjust size-in-pixels 'fullscreen)

@(define-aspect-select client-w get-client-w 'fullscreen)
@(define-aspect-select client-h get-client-h 'fullscreen)
@(define-aspect-select full-page full-page 'fullscreen)
@(define-aspect-select titleless-page titleless-page 'fullscreen)
