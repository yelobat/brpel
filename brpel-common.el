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

(defvar brpel--registry-schema-cache nil
  "A cache to the BRP registry schema.")

(defvar brpel--registry-schema-index (make-hash-table :test #'equal)
  "The Registry Schema Hash Table.")

(declare-function brpel--registry-schema-index-populate "brpel-methods")

(defun brpel-type-path (name)
  "Obtain the typePath for component/resource called NAME.
If the registry schema index has not been populated yet, populate it
first by querying the BRP server.  Return nil if NAME is unknown."
  (when (and (zerop (hash-table-count brpel--registry-schema-index))
             (fboundp 'brpel--registry-schema-index-populate))
    (ignore-errors (brpel--registry-schema-index-populate)))
  (gethash name brpel--registry-schema-index))

(defun brpel-map-type-paths (names)
  "Map NAMES into typePaths."
  (mapcar #'brpel-type-path names))

;; Response accessors

(defun brpel-result (response)
  "Return the result field of a BRP RESPONSE, or nil."
  (alist-get 'result response))

(defun brpel-error (response)
  "Return the error field of a BRP RESPONSE, or nil if it succeeded."
  (alist-get 'error response))

(defun brpel-error-message (response)
  "Return the error message of a BRP RESPONSE, or nil if it succeeded."
  (alist-get 'message (brpel-error response)))

(defun brpel-check (response)
  "Return the result field of a BRP RESPONSE.
Signal a `user-error' if RESPONSE contains a BRP error, or if it is nil
\(which usually means the response could not be parsed)."
  (cond
   ((null response)
    (user-error "BRP error: no response from %s"
                (bound-and-true-p brpel-request-url)))
   ((brpel-error response)
    (user-error "BRP error: %s" (brpel-error-message response)))
   (t (brpel-result response))))

(provide 'brpel-common)
;;; brpel-common.el ends here
