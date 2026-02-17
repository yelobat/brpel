;;; brpel-resources.el --- Examples of everything relating to resources in brpel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 17, 2026
;; Modified: February 17, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yelobat/brpel
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'brpel-helper)

;; Spawn a Camera2d
(spawn-entity (list (Camera2d)))

;; Set the ClearColor to black via mutation.
(mutate-resource (brpel-type-path "ClearColor") "0" (Srgba :r 0.0 :g 0.0 :b 0.0 :a 1.0))

;; Set the ClearColor to gray via insertion.
(insert-resource (brpel-type-path "ClearColor") (Srgba :r 0.2 :g 0.2 :b 0.2 :a 1.0))

(provide 'brpel-resources)
;;; brpel-resources.el ends here
