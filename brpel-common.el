;;; brpel-common.el --- Common variables and functions for brpel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 11, 2026
;; Modified: February 11, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yelobat/brpel
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(defgroup brpel nil
  "Interface with a BRP server from Emacs."
  :group 'brpel)

(defvar brpel--registry-schema-index (make-hash-table :test #'equal)
  "The Registry Schema Hash Table.")

(defun brpel-type-path (name)
  "Obtain the typePath for component/resource called NAME."
  (gethash name brpel--registry-schema-index))

(defun brpel-map-type-paths (names)
  "Map NAMES into typePaths."
  (mapcar #'brpel-type-path names))

(provide 'brpel-common)
;;; brpel-common.el ends here
