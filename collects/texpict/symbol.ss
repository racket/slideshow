(module symbol (lib "slideshow.ss" "slideshow")
  
  (provide symbol
	   sym:in sym:rightarrow sym:infinity sym:times 
	   sym:implies sym:emdash)
  
  (define (symbol n)
    (text (string (integer->char n)) 'symbol font-size))

  (define sym:in (symbol 206))
  (define sym:rightarrow (symbol 174))
  (define sym:infinity (symbol 165))
  (define sym:times (symbol 180))
  (define sym:implies (symbol 222))
  (define sym:emdash (symbol 190)))






