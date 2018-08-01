#lang scribble/manual
@(require (for-label slideshow/widescreen/base)
          "adjust-aspect.rkt")

@title[#:tag "widescreen"]{Widescreen Slides}

@declare-exporting[slideshow/widescreen/base slideshow/widescreen]

@defmodulelang*/no-declare[(slideshow/widescreen)]
@defmodule*/no-declare[(slideshow/widescreen/base)]

The @racketmodname[slideshow/widescreen/base] module is reprovided by
the @racket[slideshow/widescreen] language along with
@racketmodname[racket] and @racketmodname[pict].

@history[#:added "1.5"]

@(declare-aspect-adjust slide 'widescreen)
@(declare-aspect-adjust para 'widescreen)
@(declare-aspect-adjust item 'widescreen)
@(declare-aspect-adjust subitem 'widescreen)
@(declare-aspect-adjust make-outline 'widescreen)
@(declare-aspect-adjust size-in-pixels 'widescreen)

@(define-aspect-select client-w get-client-w 'widescreen)
@(define-aspect-select client-h get-client-h 'widescreen)
@(define-aspect-select full-page full-page 'widescreen)
@(define-aspect-select titleless-page titleless-page 'widescreen)
