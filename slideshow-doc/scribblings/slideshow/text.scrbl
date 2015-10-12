#lang scribble/manual

@(require (for-label slideshow/base pict slideshow/text))

@title{Text Formatting Helpers}

@defmodule[slideshow/text]

This module provides conveniences functions for formatting text.

@defform[(with-size size expr)]{

Sets @racket[current-font-size] to @racket[size] while running @racket[expr].

@history[#:added "1.2"]{}
}

@defform[(with-scale scale expr)]{

Multiplies @racket[current-font-size] by @racket[scale] while running
@racket[expr].

@history[#:added "1.2"]{}
}

@deftogether[(
@defform[(big text)]
@defform[(small text)]
)]{

Scale @racket[current-font-size] by @racket[3/2] or @racket[2/3], respectively,
while running @racket[text].

@history[#:added "1.2"]{}
}

@defform[(with-font font expr)]{

Sets @racket[current-main-font] to @racket[font] while running @racket[expr].

@history[#:added "1.2"]{}
}

@defform[(with-style style expr)]{

Adds @racket[style] to @racket[current-main-font] (via @racket[cons]) while
running @racket[expr].

@history[#:added "1.2"]{}
}

@deftogether[(
@defform[(bold text)]
@defform[(italic text)]
@defform[(subscript text)]
@defform[(superscript text)]
@defform[(caps text)]
)]{

Adds the attributes for bold, italic, superscript, subscript, or small caps
text, respectively, to @racket[current-main-font] while running @racket[text].

@history[#:added "1.2"]{}
}

@defproc[(blank-line) pict?]{
Adds a blank line of the current font size's height.

@history[#:added "1.2"]{}
}
