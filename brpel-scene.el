;;; brpel-scene.el --- Scene saving and loading library for brpel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 12, 2026
;; Modified: February 12, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yelobat/brpel-scene
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; TODO This is an experimental API
;;
;;; Code:

(require 'brpel-methods)

;; TODO Experimental API
(defun brpel-save-scene (path &optional allowed-components callback)
  "Save the current established scene to PATH.
ALLOWED-COMPONENTS specifies the components that are allowed to be saved.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-world-insert-resources
   (brpel-type-path "SaveRequest")
   `((path . ,(expand-file-name path))
     (allowed_components . ,(vconcat allowed-components))) callback))

;; TODO Experimental API
(defun brpel-save-scene-synchronously (path &optional allowed-components)
  "Save the current established scene to PATH.
ALLOWED-COMPONENTS specifies the components that are allowed to be saved."
  (brpel-world-insert-resources-synchronously
   (brpel-type-path "SaveRequest")
   `((path . ,(expand-file-name path))
     (allowed_components . ,(vconcat allowed-components)))))

(provide 'brpel-scene)
;;; brpel-scene.el ends here
