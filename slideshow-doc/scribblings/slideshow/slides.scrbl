#lang scribble/doc
@(require "ss.rkt"
          (for-label (except-in racket/gui drop)
                     slideshow/step
                     slideshow/slides-to-picts))

@(define d=> @elem['rarr])

@title[#:style 'toc]{Making Slides}

@declare-exporting[slideshow/base slideshow]

@defmodule*/no-declare[(slideshow/base)]{The
@racketmodname[slideshow/base] module, which is re-provided by
@racketmodname[slideshow], provides the functions for creating slides.}

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@section{Primary Slide Functions}

@defproc[(slide [#:title title (or/c #f string? pict?) #f]
                [#:name  name (or/c #f string?) title]
                [#:aspect aspect aspect? #f]
                [#:layout layout (or/c 'auto 'center 'top 'tall) 'auto]
                [#:gap-size sep-gap-size real? (current-gap-size)]
                [#:inset inset slide-inset? (make-slide-inset 0 0 0 0)]
                [#:timeout secs (or/c #f real?) #f]
                [#:condense? condense? any/c (and secs #t)]
                [element (flat-rec-contract elem/c
                           (or/c pict? 
                                'next 'next! 'alts 'alts~ 'nothing
                                comment?
                                (listof (listof elem/c))))] ...)
          void?]{

Creates and registers a slide. See @secref["staging"] for
information about @racket[element]s. Multiple @racket[element] picts are
separated by @racket[sep-gap-size] vertical space.

When this function is first called in non-printing mode, then the
viewer window is opened. Furthermore, each call to the function
@racket[yield]s, so that the viewer window can be refreshed, and so
the user can step through slides. If the user closes the slide window,
then @racket[slide] triggers an error unless
@racket[set-allow-new-slides-after-close!] was called with a true
value before the window was closed.

If @racket[title] is not @racket[#f], then a title is shown for the
slide. The @racket[name] is used in the slide-navigation dialog, and
it defaults to @racket[title].

If @racket[layout] is @racket['top], then the content is top-aligned,
with @racket[(* 2 sep-gap-size)] space between the title and the
content. The @racket['tall] layout is similar, but with only
@racket[sep-gap-size] space. The @racket['center] mode centers the content
(ignoring space consumed by the title). The @racket['auto] mode is
like @racket['center], except when @racket[title] is non-@racket[#f]
and when the space between the title and content would be less than
@racket[(* 2 sep-gap-size)], in which case it behaves like @racket['top].

The @racket[inset] argument supplies an inset that makes the
slide-viewing window smaller when showing the slide. See
@racket[make-slide-inset] for more information.

If @racket[secs] argument for @racket[#:timeout] is not @racket[#f],
then the viewer automatically advances from this slide to the next
after @racket[secs] seconds, and manual advancing skips this slide.

If @racket[condense?] is true, then in condense mode (as specified by
the @Flag{c} command-line flag), the slide is not created and
registered.

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}


@defproc[(t [str string?]) pict?]{

The normal way to make plain text. Returns @racket[(text str
(current-main-font) (current-font-size))].}

@defproc[(it [str string?]) pict?]{

The normal way to make italic text. Returns @racket[(text str (cons
'italic (current-main-font)) (current-font-size))].}

@defproc[(bt [str string?]) pict?]{

The normal way to make bold text. Returns @racket[(text str (cons
'bold (current-main-font)) (current-font-size))].}

@defproc[(bit [str string?]) pict?]{

Bold-italic text. Returns @racket[(text str (list* 'bold 'italic
(current-main-font)) (current-font-size))].}

@defproc[(tt [str string?]) pict?]{

The normal way to make monospaced text. Returns @racket[(text str
`(bold . modern) (current-font-size))].}

@defproc[(rt [str string?]) pict?]{

The normal way to make serif text. Returns @racket[(text str 'roman
(current-font-size))].}

@defproc[(titlet [str string?]) pict?]{

Creates title text. Returns @racket[((current-titlet) str)].}

@defproc[(para [#:aspect aspect aspect? #f]
               [#:width width real? ((get-current-para-width #:aspect aspect))]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:fill? fill? any/c #t]
               [#:decode? decode? any/c #t]
               [element (flat-rec-contract elem/c
                          (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Generates a paragraph pict that is no wider than @racket[width] units,
and that is exactly @racket[width] units if @racket[fill?] is true. If
@racket[fill?] is @racket[#f], then the result pict is as wide as the
widest line.

Each list within @racket[element]s is spliced into the sequence of
string and pict elements. If @racket[decode?] is true, then strings
among the @racket[element]s are decoded by performing the following
substitutions: @litchar{---} @d=> @litchar["\u2014"], @litchar{--}
@d=> @litchar["\u2013"], @litchar{``} @d=> @litchar["\u201C"],
@litchar{''} @d=> @litchar["\u201D"], @litchar{'} @d=>
@litchar["\u2019"]. In addition, to better work with
@racketmodname[at-exp] notation, if an @racket[element] is @racket["\n"],
then it is dropped along with any spaces at the start of the next
element.

Strings are split at spaces for word-wrapping to fit the page, and a
space is added between elements. If a string element starts with one
of the following punctuation marks (after decoding), however, no space
is added before the string:

@t{
 @hspace[2] @litchar{-} @litchar{'} @litchar{,} @litchar{.} @litchar{ } @litchar{:} 
 @litchar{;} @litchar{?} @litchar{!} @litchar{)} @litchar["\u201D"] @litchar["\u2019"]
}

The @racket[align] argument specifies how to align lines within the
paragraph.

See the spacing between lines is determined by the
@racket[current-line-sep] parameter.

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}


@defproc[(item [#:aspect aspect aspect? #f]
               [#:width width real? ((get-current-para-width #:aspect aspect))]
               [#:gap-size sep-gap-size real? (current-gap-size)]
               [#:bullet blt pict? (scale bullet (/ sep-gap-size gap-size))]
               [#:align align (or/c 'left 'center 'right) 'left]
               [#:fill? fill? any/c #t]
               [#:decode? decode? any/c #t]
               [element (flat-rec-contract elem/c
                          (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Like @racket[para], but with @racket[blt] followed by @racket[(/
sep-gap-size 2)] space appended horizontally to the resulting paragraph,
aligned with the top line. The paragraph width of @racket[blt] plus
@racket[(/ sep-gap-size 2)] is subtracted from the maximum width of the
paragraph.

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}


@defproc[(subitem [#:aspect aspect aspect? #f]
                  [#:width width real? ((get-current-para-width #:aspect aspect))]
                  [#:gap-size sep-gap-size real? (current-gap-size)]
                  [#:bullet blt pict? (scale o-bullet (/ sep-gap-size gap-size))]
                  [#:align align (or/c 'left 'center 'right) 'left]
                  [#:fill? fill? any/c #t]
                  [#:decode? decode? any/c #t]
                  [element (flat-rec-contract elem/c
                             (or/c string? pict? (listof elem/c)))] ...)
         pict?]{

Like @racket[item], but an additional @racket[(* 2 sep-gap-size)] is
subtracted from the paragraph width and added as space to the left of
the pict. Also, @racket[o-bullet] is the default bullet, instead of
@racket[bullet].

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}


@defproc[(clickback [pict pict?] [thunk (-> any)])
         pict?]{

Creates a pict that embeds the given one, and is the same size as the
given pict, but that when clicked during a presentation calls
@racket[thunk].}


@defproc[(interactive [pict pict?] [proc (frame% . -> . (-> any))])
         pict?]{

Creates a pict that embeds the given one, but that creates a floating
frame at the pict's location on the screen during a
presentation. After the floating frame is created (and before it is shown),
@racket[proc] is applied to the frame. The result from @racket[proc]
must be a procedure that is called when the window is removed (because
the slide changes, for example).}


@defproc[(size-in-pixels [pict pict?]
                         [#:aspect aspect aspect? #f])
         pict?]{

Scales @racket[pict] so that it is displayed on the screen as
@racket[(pict-width pict)] pixels wide and @racket[(pict-height pict)]
pixels tall. The result is @racket[pict] when using a 1024 by 768
display with a fullscreen aspect or when using a 1360 by 766
display with a widescreen aspect.

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}


@defproc[(pict->pre-render-pict [pict pict?]) pict?]{

Produces a pict that is like @racket[pict], but optimized for drawing
on some platforms (currently Mac OS). This function may be useful
to reduce drawing times for for large bitmaps or complex drawings.

@history[#:added "1.1"]}


@defproc[(make-outline [name (or/c symbol? (listof symbol?))]
                       [title (or/c string? pict?)]
                       [subitems (or/c #f null?
                                       (symbol? . -> . pict?))]
                       ...
                       [#:aspect aspect aspect? #f])
          (symbol? . -> . void?)]{

Returns a function that takes a symbol and generates an outline
slide.

The @racket[...] above applies to all three arguments together.  Each
trio of arguments defines a section for the outline:

@itemize[

 @item{The section @racket[name] is either a symbol or a list of symbols. When
        the outline function is called later to make an outline, the
        given symbol is compared to the section's symbol(s), and the
        section is marked as current if the symbol matches.}

 @item{The @racket[title] is used as the displayed name of the
       section.}

 @item{The @racket[subitems] are displayed when the section is
       active. It can be @racket[#f] or @racket[null] (for historical
       reasons) if no subitems are to be displayed. Otherwise, it
       should be a function that takes a symbol (the same one passed
       to the outline maker) and produces a pict.}

]

@history[#:changed "1.5" @elem{Added the @racket[#:aspect] argument.}]}

@defproc[(comment [text (or/c string? pict?)] ...)
         comment?]{

Combines strings and picts to be used as a slide element for (usually
hidden) commentary. Use the result as an argument to @racket[slide].}

@defproc[(comment? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a comment produced by
@racket[comment].}

@; ------------------------------------------------------------------------

@section{Slide Registration}

@defproc[(slide? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a slide produced by
@racket[most-recent-slide] or @racket[retract-most-recent-slide].}

@defproc[(most-recent-slide) slide?]{

Returns a slide structure that may be supplied to @racket[re-slide] to make a
copy of the slide or @racket[slide->pict] to re-extract the entire
slide as a pict.}

@defproc[(retract-most-recent-slide) slide?]{

Cancels the most recently created slide, and also returns a slide
structure that may be supplied to @racket[re-slide] to restore the slide
(usually in a later position).}

@defproc[(re-slide [slide slide?] [pict pict? (blank)])
         void?]{

Re-inserts a slide, @racket[lt-superimpose]ing the given additional
@racket[pict].}

@defproc[(slide->pict [slide slide?])
         pict?]{

Converts a complete slide to a @racket[pict]. The bounding box of the
result corresponds to the slide within its margins.}

@; ------------------------------------------------------------------------

@section{Viewer Control}

@defproc[(start-at-recent-slide) void?]{

Sets the starting slide for the talk to the most recently created
slide. If this function is used multiple times, the last use overrides
the earlier uses.}


@defproc[(enable-click-advance! [on? any/c]) void?]{

Enables or disables slide advance as a result of a mouse click.}


@defproc[(set-use-background-frame! [on? any/c]) void?]{

Enables or disables the creation of a background frame, which is
typically useful only when @racket[make-slide-inset] is used.
The last enable/disable before the first slide registration
takes effect once and for all.}

@defproc[(set-page-numbers-visible! [on? any/c]) void?]{

Determines whether slide numbers are initially visible in the viewer.}


@defparam[current-page-number-font font (is-a?/c font%)]{

Parameter that determines the font used to draw the page number (if
visible).}


@defparam[current-page-number-color color (or/c string? (is-a?/c color%))]{

Parameter that determines the color used to draw the page number (if
visible).}

@defparam[current-page-number-adjust proc (-> number? string? string?)]{ 
Parameter that controls the precise text
that appears to indicate the page numbers (if visible). The
input to the function is the default string and the slide
number, and the result is what is drawn in the bottom right
corner. The default parameter value just returns its second
argument.
}

@defproc[(set-spotlight-style! [#:size size (or/c #f (>=/c 0)) #f]
                               [#:color color (or/c #f string? (is-a?/c color%)) #f])
         void?]{

Adjusts the size and color of the ``spotlight,'' which can be enabled
in Slideshow as an alternative to the mouse. Note that the color
normally should have alpha value less than 1 (to make it partially
transparent). If @racket[size] or @racket[color] is @racket[#f], the
corresponding setting is unchanged.}


@defproc[(set-allow-new-slides-after-close! [on? any/c])
         void?]{

Sets whether new slides are allowed after the Slideshow window is
closed by the user. By default, an attempt to register a new slide via
@racket[slide] after the window is closed triggers an error. Calling
this function with @racket[#t] enables new slides to start a new slideshow.

@history[#:added "1.3"]}

@; ------------------------------------------------------------------------

@section{Constants and Layout Variables}

@defproc[(aspect? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] is @racket['fullscreen],
@racket['widescreen], or @racket[#f], otherwise returns @racket[#f].

A symbolic @racket[v] selects a specific aspect, while @racket[#f] as
an aspect corresponds to a user-selected aspect through the
@DFlag{widescreen} or @DFlag{fullscreen} flag.

See also @seclink["aspect"].

@history[#:added "1.5"]}


@defthing[gap-size 24]{

A width commonly used for layout.}


@defparam[current-gap-size sep-gap-size real?]{

A parameter whose value is a width used for the separation between
items by @racket[slide], the size and spacing of a bullet for
@racket[item], the space between a slide title and content in
@racket['tall] mode, etc.  The default value is @racket[gap-size].}


@defthing[bullet pict?]{

A filled bullet used by default by @racket[item].

It is either @racket[(t "\u2022")], if that character
is available in the font that @racket[t] uses,
or it uses an implementation similar to @racket[o-bullet],
but not hollow (using @racket[disk], not @racket[circle]).
}


@defthing[o-bullet pict?]{

A hollow bullet used by default by @racket[subitem].

It's implementation is:
@racketblock[(baseless
              (cc-superimpose 
               (circle (/ gap-size 2)) 
               (blank 0 gap-size)))]
}

@deftogether[(
@defidform[client-w]
@defproc[(get-client-w [#:aspect aspect aspect? #f]) exact-nonnegative-integer?]
)]{

Produces the width of the display area, minus @racket[margin]s for a
given @racket[aspect], where @racket[client-w] is equivalent to
@racket[(get-client-w)]. The result changes if the margin is adjusted
via @racket[set-margin!].

@history[#:changed "1.5" @elem{Added @racket[get-client-w].}]}


@deftogether[(
@defidform[client-h]
@defproc[(get-client-h [#:aspect aspect aspect? #f]) exact-nonnegative-integer?]
)]{

Produces the height of the display area, minus @racket[margin]s for a
given @racket[aspect], where @racket[client-h] is equivalent to
@racket[(get-client-h)]. The result changes if the margin is adjusted
via @racket[set-margin!].

@history[#:changed "1.5" @elem{Added @racket[get-client-h].}]}


@deftogether[(
@defidform[full-page]
@defproc[(get-full-page [#:aspect aspect aspect? #f]) pict?]
)]{

Produces an empty pict that is the same size as the client area, which
is like @racket[(blank client-w client-h)]. The @racket[full-page]
form is equivalent to @racket[(get-full-page)].

@history[#:changed "1.5" @elem{Added @racket[get-full-page].}]}


@deftogether[(
@defidform[titleless-page]
@defproc[(get-titleless-page [#:aspect aspect aspect? #f]) pict?]
)]{

Produces an empty pict that is the same size as the client area minus
the title area in @racket['top] layout mode, which is like
@racket[(blank client-w (- client-h title-h (* 2 gap-size)))]. The
@racket[titleless-page] form is equivalent to
@racket[(get-titleless-page)].

@history[#:changed "1.5" @elem{Added @racket[get-titleless-page].}]}


@defidform[margin]{

Produces a number that corresponds to the current margin, which
surrounds every side of the slide. The client area for a slide
corresponds to the display area (which is always 1024 by 768) minus
this margin on each side. The default margin is @racket[20].

The margin can be adjusted via @racket[set-margin!].}


@defidform[title-h]{

Produces a number that corresponds to the height of a title created by
@racket[titlet].

If @racket[titlet] is changed via the @racket[current-titlet]
parameter, the title height should be updated via
@racket[set-title-h!].}


@defthing[printing? boolean?]{

The value is @racket[#t] if slides are being generated for printed
output, @racket[#f] for normal on-screen display. Printing mode is
normally triggered via the @DFlag{print} or @DFlag{ps} command-line
flag.}


@defthing[condense? boolean?]{

The value is @racket[#t] if slides are being generated in condensed
mode, @racket[#f] for normal mode. Condensed mode is normally
triggered via the @DFlag{condense} command-line flag.}

@; ------------------------------------------------------------------------

@section{Configuration}

@defparam[current-font-size n exact-nonnegative-integer?]{

Parameter that determines the font size used by @racket[t],
@racket[para], etc. The default size is @racket[32].}


@defparam[current-main-font style text-style/c]{

Parameter that determines the font size used by @racket[t],
@racket[para], etc.  The default is platform-specific; possible
initial values include @racket['swiss], @racket["Verdana"], and
@racket["Gill Sans"].}


@defparam[current-line-sep n exact-nonnegative-integer?]{

Parameter that controls the amount of space used between lines by
@racket[para], @racket[item], and @racket[subitem].}


@deftogether[(
@defparam[current-para-width n exact-nonnegative-integer?]
@defproc[(get-current-para-width (#:aspect aspect aspect? #f))
         (parameter/c exact-nonnegative-integer?)]
)]{

Parameter that controls the width of a pict created by
@racket[para], @racket[item], and @racket[subitem]. The value
of @racket[current-para-width] is the same as
@racket[(get-current-para-width)].

@history[#:changed "1.5" @elem{Added @racket[get-current-para-width].}]}


@defparam[current-title-color color (or/c string? (is-a?/c color%))]{

Parameter used by the default @racket[current-titlet] to colorize the
title. The default is @racket["black"].}


@defparam[current-slide-assembler proc ((or/c string? #f)
                                        exact-nonnegative-integer?
                                        pict?
                                        . -> .
                                        pict?)]{

Parameter whose value is a function for assembling slide content into
a single pict; the assembling function takes a string for the title
(or @racket[#f]), a separation for the title (if any) and pict, and a
pict for the slide content (not counting the title).

The result is of the assembler is @racket[ct-superimpose]d with the
client area, but the result pict might draw outside the client region
to paint the screen margins, too.

The default assembler uses @racket[titlet] to turn a title string (if
any) to a pict. See also @racket[current-titlet] and
@racket[set-title-h!].

The slide assembler is @emph{not} responsible for adding page
numbers to the slide; that job belongs to the viewer. See also
@racket[current-page-number-font], @racket[current-page-number-color],
and @racket[set-page-numbers-visible!].}


@defparam[current-titlet proc (string? . -> . pict?)]{

Parameter to configure @racket[titlet]. The default is

@racketblock[
 (lambda (s)
   (colorize (text s (current-main-font) 40)
             (current-title-color)))
]

If this parameter is changed such that the result is a different
height, then @racket[set-title-h!] should be called to update the
value produced by @racket[title-h], @racket[titleless-page], etc.}


@defproc[(set-margin! [amt real?]) void?]{

Changes the margin that surrounds the client area. See also
@racket[margin].}


@defproc[(set-title-h! [amt real?]) void?]{

Changes the expected height of a title, which adjusts @racket[title-h],
@racket[client-h], @racket[full-page], and @racket[titleless-page].}


@defproc[(make-slide-inset [left-inset exact-nonnegative-integer?]
                           [top-inset exact-nonnegative-integer?]
                           [right-inset exact-nonnegative-integer?]
                           [bottom-inset exact-nonnegative-integer?])
          slide-inset?]{

Creates a slide inset, which describes a number of pixels to inset the
viewer for a slide on each side.}


@defproc[(slide-inset? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a slide inset created by
@racket[make-slide-inset], @racket[#f] otherwise.}

@defparam[commentary-on-slide-font-size size exact-positive-integer?]{
 The font size used for commentary when passing @verbatim{--commentary-on-slide}
 on the command-line.
}

@; ----------------------------------------------------------------------

@section{Pict-Staging Helper}

@defmodule[slideshow/step]{The @racketmodname[slideshow/step] library
provides syntax for breaking a complex slide into steps that are more
complex than can be handled with @racket['next] and @racket['alts] in
a @racket[slide] sequence.}

@defform[(with-steps (id ...) body ...)]{

Evaluates the @racket[body]s once for each @racket[id], skipping an
@racket[id] if its name ends with @litchar{~} and @racket[condense?]
is true. The results of the last @racket[body] for each iteration are
collected into a list, which is the result of the @racket[with-steps]
form.

Within the @racket[body]s, several keywords are bound non-hygienically
(using the first @racket[body]'s lexical context):

@itemize[

 @item{@racket[(only? id)] --- returns @racket[#t] during the
    @racket[id] step (i.e., during the evaluation of the
    @racket[body]s for @racket[id]), @racket[#f] otherwise.}

 @item{@racket[(vonly id)] --- returns the identity function during
    the @racket[id] step, @racket[ghost] otherwise.}

 @item{@racket[(only id _then-expr)] returns the result of
    @racket[_then-expr] during the @racket[id] step, @racket[values]
    otherwise.}

 @item{@racket[(only id _then-expr _else-expr)] returns the result
    of @racket[_then-expr] during the @racket[id] step, the result of
    @racket[_else-expr] otherwise.}

 @item{@racket[(before? id)] --- returns @racket[#t] before the
    @racket[id] step, @racket[#f] starting for the @racket[id] and
    afterward.}

 @item{@racket[(vbefore id)], @racket[(before id _then-expr)], or
    @racket[(before id _then-expr _else-expr)] --- analogous to
    @racket[vonly] and @racket[only].}
    
 @item{@racket[(after? id)] --- returns @racket[#t] after the
    @racket[id] step, @racket[#f] through the @racket[id] step.}

 @item{@racket[(vafter id)], @racket[(after id _then-expr)], or
    @racket[(after id _then-expr _else-expr)] --- analogous to
    @racket[vonly] and @racket[only].}
    
 @item{@racket[(between? _a-id _b-id)] --- returns @racket[#t]
    starting from the @racket[_a-id] step through the @racket[_b-id]
    step, @racket[#f] otherwise.}

 @item{@racket[(vbetween _a-id _b-id)], @racket[(between _a-id
    _b-id _then-expr)], or @racket[(between _a-id _b-id _then-expr
    _else-expr)] --- analogous to @racket[vonly] and @racket[only].}

 @item{@racket[(between-excel? _a-id _b-id)] --- returns
    @racket[#t] starting from the @racket[_a-id] step through steps
    before the @racket[_b-id] step, @racket[#f] for the @racket[_b-id]
    step and afterward.}

 @item{@racket[(vbetween-excl _a-id _b-id)], @racket[(between-excl
    _a-id _b-id _then-expr)], or @racket[(between-excl _a-id _b-id
    _then-expr _else-expr)] --- analogous to @racket[vonly] and
    @racket[only].}
]}


@defform[(with-steps~ (id ...) body ...)]{

Like @racket[with-steps], but when @racket[condense?] is true, then
@racket[expr] is evaluated only for the last @racket[id] (independent
of whether the name of the last @racket[id] name ends in @litchar{~}).
}

@; ----------------------------------------------------------------------

@include-section{text.scrbl}

@; ----------------------------------------------------------------------

@section{Slides to Picts}

@defmodule[slideshow/slides-to-picts]

@defproc[(get-slides-as-picts [path path-string?]
                              [width real?]
                              [height real?]
                              [condense? any/c]
                              [stop-after (or/c #f exact-nonnegative-integer?) #f])
         (listof pict?)]{

Executes the Slideshow program indicated by @racket[path] in a fresh
namespace, and returns a list of picts for the slides. Each pict has
the given @racket[width] and @racket[height], and @racket[condense?]
determines whether the Slideshow program is executed in condense
mode.

If @racket[stop-after] is not @racket[#f], then the list is truncated
after @racket[stop-after] slides are converted to picts.}

