;; This example slideshow code is meant to be read while
;;  running the example show.
;; The code doesn't try to show itself in the slides, because
;;  that would be tricky (defeating the point of having
;;  a simple tutorial)

(module example-show (lib "slideshow-run.ss" "texpict")

  ;; Because we want to typeset Scheme code:
  (require (lib "slide-code.ss" "texpict"))
  
  ;; The first slide
  (slide
   (t "Part I: Basic Concepts"))

  ;; The second slide
  (slide
   (t "Read the code while viewing these slides"))
  
  ;; Etc.
  (slide
   (t "This slide shows how four picts")
   (t "get vertically appended by the")
   (tt "slide")
   (t "function to create a slide"))
  
  (slide
   (t "See how the")
   (tt "t")
   (t "function takes a string and")
   (t "produces a pict with a normal sans-serif font, but")
   (tt "tt")
   (t "produces a pict with a fixed-width font?"))
  
  (slide
   (page-para "Breaking up text into lines is painful, so"
              "the" (tt "page-para")
              "function takes a mixture of strings and"
              "picts and puts them into a pagaraph")
   (page-para "It doesn't matter how strings are broken into"
              "parts in the code")
   (page-para "The" (tt "page-para") "function puts space"
              "between separate strings"
              ", but not before punctuation" "!"))
  
  (slide/center
   (page-para "The" (tt "slide/center") "function centers"
              "the slide body vertically")
   (page-para "All of the" (tt "slide") "functions center"
              "the body picts horizontally, but" (tt "page-para")
              "makes a slide-width picture with left-aligned text")
   (frame (page-para "The" (tt "frame") "function wraps a frame"
                     "around a pict to create a new pict,"
                     "so you can easily this this individual pict")))
  
  (slide/title
   "Titles"
   (page-para "The" (tt "slide/title") "function takes a"
              "title string before the content picts"))
  
  (slide/title/center
   "Titles and Centering"
   (page-para "The" (tt "slide/title/center") "function is obvious..."))
  
  (slide/title/center
   "More Centering"
   (frame
    (page-para/c "The" (tt "page-para/c") "function generates"
                 "a paragraph with centered lines of text"))
   (frame
    (page-para* "This line uses the" (tt "page-para*") "function"))
   (page-para "The" (tt "page-para*") "function creates a paragraph"
              "that is wrapped to fit the slide, but it allows"
              "the resulting pict to be more narrow than the slide"))

  (slide/title
   "More Alignment"
   (frame
    (page-para/r "Of course, there's also" (tt "page-para/r")))
   (frame
    (page-para*/r "And there's" (tt "page-para*/r") ", which is different"
                  "from" (tt "page-para*") "or" (tt "page-para*/c")
                  "only if the paragraph takes multiple lines"))
   (page-para "For comparision, the same text using" (tt "page-para/r") ":")
   (frame
    (page-para/r "And there's" (tt "page-para*/r") ", which is different"
                 "from" (tt "page-para*") "or" (tt "page-para*/c")
                 "only if the paragraph takes multiple lines"))
   (page-para "Unless your font happens to make the" (tt "page-para*/r")
              "box exactly as wide as this slide, the last box will be slightly"
              "wider with extra space to the left"))
  
  (slide/title
   "Spacing"
   (page-para "The" (tt "slide") "functions insert space"
              "between each body pict")
   (page-para "The amount of space is" (number->string gap-size)
              ", which is the value of" (tt "gap-size")))
  
  (slide/title
   "Controlling Space"
   (vc-append line-sep
              (page-para "If you want to control the space,"
                         "simply append the picts yourself to create"
                         "one body pict")
              (page-para "The first argument to" (tt "vc-append")
                         "determines the space between pictures")
              (page-para "If the first argument is a pict instead of"
                         "a number, then 0 is used")
              (page-para "For text in one paragraph, the"
                         (tt "page-para") "function uses" (tt "line-sep")
                         ", which is" (number->string line-sep))))

  (slide/title/center
   "Appending Picts"
   (vl-append line-sep (frame (t "This is")) (frame (tt "vl-append")))
   (vc-append line-sep (frame (t "This is")) (frame (tt "vc-append")))
   (vr-append line-sep (frame (t "This is")) (frame (tt "vr-append"))))
  
  (slide/title/center
   "Horizontal Appending"
   (hc-append (frame (t "This is")) (frame (vr-append (tt "hc-append")
                                                      (t "obviously"))))
   (ht-append (frame (t "This is")) (frame (vr-append (tt "ht-append")
                                                      (t "obviously"))))
   (hb-append (frame (t "This is")) (frame (vr-append (tt "hb-append")
                                                      (t "obviously")))))
  
  (slide/title/center
   "Text Alignment"
   (hbl-append (frame (scale (tt "hbl-append") 1.5))
               (frame (t "aligns text baselines")))
   (page-para "It's especially useful for font mixtures")
   (hbl-append (frame (scale (tt "htl-append") 1.5))
               (frame (t "is the same for single lines")))
   (page-para "The difference between" (tt "htl-append")
              "and" (tt "hbl-append") "shows up with multiple lines:")
   (hbl-append (frame (scale (t "bottom lines align") 1.5))
               (frame (vl-append (t "when using") (tt "hbl-append"))))
   (htl-append (frame (scale (t "top lines align") 1.5))
               (frame (vl-append (t "when using") (tt "htl-append")))))
  
  (slide/title
   "Superimposing"
   (cc-superimpose (t "X") (t "O"))
   (page-para "The" (tt "cc-superimpose") 
              "function puts picts on top of each other, centered")
   (page-para "Each of" (tt "l") "," (tt "r") ", and" (tt "c")
              "is matched with each of"
              (tt "t") "," (tt "b") "," (tt "c") "," (tt "bl") ", and" (tt "tl")
              "in all combinations with" (tt "-superimpose"))
   (page-para "For example," (tt "cbl-superimpose") ":")
   (cbl-superimpose (frame (scale (t "one line") 1.5))
                    (frame
                     (colorize (vl-append (t "two")
                                          (t "lines"))
                               "blue"))))
  
  (slide
   (cc-superimpose 
    (frame (blank client-w client-h))
    (vc-append gap-size
               (t "By definition, the screen is 1024 x 768 units")
               (t "If you have more or less pixels, the image is scaled")
               (page-para*
                "There's a margin, so the \"client\" area is"
                (number->string client-w) "x" (number->string client-h))
               (page-para* "The font size is" (number->string font-size)))))
  
  (slide/title
   "Titled Client Area"
   (rb-superimpose
    (cc-superimpose
     (frame titleless-page)
     (page-para* "If you use a title, then" (tt "titleless-page")
                 "is the same size as the area left for the body"))
    (t "Yes, it's useful")))
   
  (slide/title
   "More on Paragraphs"
   (page-para "The" (tt "page-") "in" (tt "page-para")
              "makes the paragraph take the width of the slide")
   (para 300 
         "The" (tt "para") "function requires an explicit size"
         "for the width of the paragraph, 300 in this case")
   (para client-w "So" (tt "page-para") "is a shorthand for"
         (tt "para") "with" (tt "client-w"))
   (para*/c 500 "Naturally, there is" (tt "para*") ","
            (tt "para*/c") ", and" (tt "para*/r")))
  
  (slide/title/center
   "Text and Styles"
   (page-para "Functions exist for" (bt "bold") ","
              (it "italic") ", and even" (bit "bold-italic") "text")
   (page-para "The" (tt "text") "function lets you change the"
              (text "font" '(italic . modern) font-size) ","
              (text "size" main-font 24) ", and even"
              (text "angle" main-font font-size (/ 3.14159 4))))

  (slide/title/center
   "Scheme Code"
   (page-para "For Scheme code, the" (code (lib "slide-code.ss" "texpict"))
              "library provides a handy" (code code) "macro for"
              "typesetting literal code")
   (page-para "The" (code code) "macro uses source-location information"
              "to indent code")
   (code (define (length l)
           (cond
             [(null? l) 0]
             [else (+ 1 (length (cdr l)))]))))

  (slide/title/center
   "Colors"
   (page-para "Use the" (colorize (tt "colorize") "blue")
              "function to color most things, including text")
   (frame
    (colorize (page-para "A" (code colorize)
                         "applies only to sub-picts that do not"
                         "already have a" (colorize (t "color") "green"))
              "red")))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Part II starts here
  
  (slide/center
   (titlet "Part II: Practical Slides"))
  
  (slide/title/center
   "Itemize"
   (page-item "Bulleted sequences are common in slides")
   (page-item "The" (code page-item) "function makes a bulleted"
              "paragraph that is as wide as the slide")
   (page-item/bullet (colorize (tt "+") "green")
                     "You can set the bullet, if you like, by using"
                     (code page-item/bullet))
   (page-subitem "Naturally, there is also" (code page-subitem)))
  
  (slide/title/center
   "Itemize"
   (page-para "You could write" (code page-item) "yourself, of course:")
   (code (define (page-item . l)
           (htl-append
            (/ gap-size 2)
            bullet
            (apply para 
                   (- client-w (pict-width bullet) 
                      (/ gap-size 2))
                   l))))
   (page-para "where" (code bullet) "is a constant pict:" bullet))
  
  (slide/title/center
   "Grouping and Space"
   (page-para "Sometimes you want to group items on a slide")
   (page-item "A bullet goes with a statement")
   (page-item "And another does, too")
   (blank)
   (page-para "Creating a zero-sized empty pict with" (code (blank))
              "inserts a space that often looks right"))
  
  (slide/title/center
   "Steps"
   (page-item "Suppose you want to show only one item at a time")
   'next
   (page-item "The" (code slide) "functions actually accept more"
              "than just picts for the body")
   (page-item "Use" (code 'next) "in the argument sequence"
              "to create multiple slides, one"
              "containing only the preceding picts, and another"
              "with the remainder")
   'next
   (blank)
   (colorize
    (page-para* (code 'next) "is not tied to" (code page-item)
                ", though it's often used with items")
    "blue"))
    
  
  (slide/title/center
   "Steps and Printing"
   (page-item "If you" (bit "condense") "these slides,"
              "the previous slide's steps will be skipped")
   'next!
   (page-item "Not this slide's steps, because it uses" (code 'next!))
   'next!
   (page-item "Condensing is often useful when printing slides:")
   (tt "slideshow --print --condense ..."))

  
  (slide/title/center
   "Alternatives"
   (page-para "Stepping stages a single slide, but sometimes"
              "you need to replace one thing with something else")
   'alts 
   (list (list 
          (page-para* "For example, replace this..."))
         (list
          (page-para* "... with something else")
          'next
          (blank)
          (page-item "An" (code 'alts) "in a sequence"
                     "must be followed by a list of lists")
          (page-item "Each list is a different conclusion for the slide's sequence")))
   (page-item "Anything after the list is folded into the last alternative")
   'next
   (blank)
   (page-para "Of course, you can mix" (code 'alts) "and"
              (code 'next) "in interesting ways"))
  
  (slide/title/center
   "Condensing Alternative"
   (page-para "Condensing" (it "does not") "merge" (code 'alts) "in any way")
   (page-para "But sometimes you want condensing to just use the last alternative")
   'alts~
   (list (list (t "um..."))
         (list (page-para
                (code 'alts~) "creates alternatives where only the last one"
                "is used when condensing"))))
  
  (define outline 
    (make-outline
     'one "Part I" #f
     'two "Part II" (lambda (tag)
                      (page-subitem "Oops, we did that already"))
     'end "Conclusion" (lambda (tag)
                         (page-subitem "This is the end"))))
  (outline 'two)
  (outline 'end)

  (slide/title/center
   "Your Own Slides"
   (page-para "A slideshow presentation is a Scheme program in a module,"
              "so to make your own:")
   (page-item "Start DrScheme")
   (page-item "Select the" (bt "(module ...)") "language from DrScheme's"
              (bt "Choose Language") "dialog")
   (page-item "Write your code:")
   (scale/improve-new-text
    (code (module _my-show (lib "slideshow-run.ss" "texpict")
            ... #,(it "your code here") ...))
    0.75))

  (slide/title/center
   "Where to Go From Here"
   (page-para "Now you've seen the basics")
   (page-item "There are" (it "many") "other functions for creating picts")
   (page-subitem "See the docs")
   (page-item "There are a few other slide features, such as comments and transitions")
   (page-subitem "Again, see the docs"))
  
  
  )
