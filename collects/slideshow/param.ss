
(module param mzscheme
  (require "cmdline.ss"
	   "viewer.ss")

  (provide current-config
	   current-viewer)

  (define current-config (make-parameter cmdline@))
  (define current-viewer (make-parameter viewer@)))

