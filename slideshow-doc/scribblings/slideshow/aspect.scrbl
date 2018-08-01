#lang scribble/doc
@(require "ss.rkt")

@title[#:tag "aspect"]{Fullscreen vs. Widescreen Aspect Ratio}

Fullscreen (4:3) versus widescreen (16:9) aspect mode is a property of
an individual slide that can be selected using the @racket[#:aspect]
argument to @racket[slide]. The @racketmodname[slideshow/widescreen]
language provides a variant of @racket[slide] that makes
@racket['widescreen] the default value of @racket[#:aspect], while
@racketmodname[slideshow/fullscreen] provides a variant of
@racket[slide] that makes @racket['fullscreen] the default.

When a slide's aspect is not specified, then it adopts an aspect that
can be selected via the @DFlag{widescreen} or @DFlag{fullscreen} flag
when Slideshow starts. (That selection can be made ``sticky'' as the
default for future runs by using the @DFlag{save-aspect} flag.)
Selecting an aspect also affects the values of @racket[client-w],
@racket[client-h], @racket[full-page], and @racket[titleless-page]
from @racketmodname[slideshow], but it does not affect the bindings
from @racketmodname[slideshow/widescreen] or
@racketmodname[slideshow/fullscreen]. Keep in mind that specifying
@racket[#:aspect] for @racket[slide] does not affect the value of
@racket[client-w], etc., for constructing the slide's content, but you
can use @racket[get-client-w], etc., to obtain the aspect-specific
metrics.

Use the @racketmodname[slideshow] language for slides and libraries
that are meant to adapt to a user's selected aspect, and use
@racketmodname[slideshow/fullscreen] or
@racketmodname[slideshow/widescreen] for slides and libraries that
assume specific values for a slide's drawing area.

@include-section["fullscreen.scrbl"]
@include-section["widescreen.scrbl"]
