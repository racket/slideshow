
(module code "slideshow.ss"
  (require (lib "code.ss" "texpict")
	   (lib "unitsig.ss"))

  (define-values/invoke-unit/sig code^
    code@
    #f
    code-params^)

  (define-code code typeset-code)

  (provide code)
  (provide-signature-elements code^))



