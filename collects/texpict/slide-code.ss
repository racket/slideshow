
(module slide-code "slideshow.ss"
  (require "code.ss"
	   (lib "unitsig.ss"))

  (define-values/invoke-unit/sig code^
    code@
    #f
    code-params^)

  (provide code)
  (provide-signature-elements code^))



