#lang scribble/doc
@(require "ss.rkt" (for-label slideshow/plt-title-background pict pict/color racket/draw))

@title[#:tag "plt-background"]{PLT Title Background}

@defmodule[slideshow/plt-title-background]{
The @racketmodname[slideshow/plt-title-background] module provides
bindings for generating slide backgrounds with the PLT logo.}

@defthing[plt-title-background pict?]{
A rendering of the PLT logo on a pict the size of the client area.}

@defproc[(make-plt-title-background [width real? client-w]
                                    [height real? client-h]
                                    [red-color
                                     (or/c color/c (-> (is-a?/c dc<%>) any))
                                     plt-red-color]
                                    [blue-color
                                     (or/c color/c (-> (is-a?/c dc<%>) any))
                                     plt-blue-color]
                                    [background-color (or/c color/c #f) plt-background-color]
                                    [lambda-color
                                     (or/c color/c (-> (is-a?/c dc<%>) any) #f)
                                     plt-lambda-color]
                                    [pen-color color/c plt-pen-color]
                                    [pen-style pen-style/c plt-pen-style]
                                    [#:clip? clip? boolean? #t]
                                    [#:edge-cleanup-pen edge-cleanup-pen (or/c (is-a?/c pen%) #f) #f])
         pict?]{
Produces a pict of the PLT logo of the specified width, height, and colors.}

@defthing[plt-red-color color/c]{The default red color used by @racket[make-plt-title-background]]}
@defthing[plt-blue-color color/c]{The default blue color used by @racket[make-plt-title-background]}
@defthing[plt-background-color color/c]{The default background color used by @racket[make-plt-title-background]}
@defthing[plt-lambda-color color/c]{The default lambda color used by @racket[make-plt-title-background]}
@defthing[plt-pen-color color/c]{The default pen color used by @racket[make-plt-title-background]}
@defthing[plt-pen-style pen-style/c]{The default pen style used by @racket[make-plt-title-background]}
