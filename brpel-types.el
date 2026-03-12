;;; brpel-types.el --- BRP Registry Schema to Elisp functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: March 10, 2026
;; Modified: March 10, 2026
;; Version: 0.0.1
;; Keywords: bevy games lisp tools
;; Homepage: https://github.com/yelobat/brpel
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This is an experimental API for turning Bevy type specifications into
;; Elisp objects, providing easier introspection capabilities from Emacs.
;;
;;; Code:

(require 'brpel-common)
(require 'brpel-methods)

;; Error conditions

(define-error 'registry-schema-invalid "Registry Schema is invalid.")

(defun brpel-types--kind (type)
  "Get the kind of a given TYPE."
  (alist-get 'kind type))

(defun brpel-types--registry-schema-result ()
  "Get the result from the registry schema.
Signal an error if the schema is nil or an error ocurred."
  (when (null brpel--registry-schema-cache)
    (signal 'registry-schema-invalid '()))
  (alist-get 'result brpel--registry-schema-cache))

(defun brpel-types--type-from-name (name)
  "Get the type spec for a given type with NAME."
  (alist-get (intern name) (brpel-types--registry-schema-result)))

(defun brpel-types--struct (type)
  "Get the Elisp type spec for a struct TYPE."
  (let ((short-path (intern (alist-get 'shortPath type)))
        (properties (alist-get 'properties type)))
    (list (append (list short-path)
                  (mapcar (lambda (field)
                            (let* ((field-type (alist-get 'type field))
                                   (type-ref (alist-get '$ref field-type))
                                   (type (car (last (split-string type-ref "/")))))
                              (cons (car field) (brpel-types-from-name type))))
                          properties)))))

(defun brpel-types--value (type)
  "Get the Elisp type spec for a value TYPE."
  (let ((short-path (intern (alist-get 'shortPath type))))
    short-path))

(defun brpel-types--enum (type)
  "Get the Elisp type spec for an enum TYPE."
  (let ((one-of (alist-get 'oneOf type))
        (field-type (alist-get 'type type)))
    (if (string= "string" field-type)
        (append one-of '())
      (mapcar (lambda (field)
                (brpel-types-from-type field))
              one-of))))

(defun brpel-types--tuple (type)
  "Get the Elisp type spec for a tuple TYPE."
  (let ((prefix-items (alist-get 'prefixItems type)))
    (mapcar (lambda (path)
              (let* ((type-path (alist-get 'type path))
                     (type-ref (alist-get '$ref type-path)))
                (brpel-types-from-name (car (last (split-string type-ref "/"))))))
            prefix-items)))

(defun brpel-types--tuple-struct (type)
  "Get the Elisp type spec for a tuple struct TYPE."
  (let ((type-path (alist-get 'typePath type)))
    (cons type-path (brpel-types--tuple type))))

(defun brpel-types--array (type)
  "Get the Elisp type spec for an array TYPE."
  (let ((short-path (alist-get 'shortPath type)))
    short-path))

(defun brpel-types--list (type)
  "Get the Elisp type spec for a list TYPE."
  (let ((short-path (alist-get 'shortPath type)))
    short-path))

(defun brpel-types--map (type)
  "Get the Elisp type spec for a map TYPE."
  (let ((short-path (alist-get 'shortPath type)))
    short-path))

(defun brpel-types--set (type)
  "Get the Elisp type spec for a set TYPE."
  (let ((short-path (alist-get 'shortPath type)))
    short-path))

(defun brpel-types-from-name (name)
  "Get the Elisp type spec for a type with NAME."
  (let* ((type (brpel-types--type-from-name name)))
    (brpel-types-from-type type)))

(defun brpel-types-from-type (type)
  "Get the Elisp type spec for a TYPE."
  (let* ((kind (brpel-types--kind type)))
    (cond
     ((string= "Tuple" kind) (brpel-types--tuple type))
     ((string= "Array" kind) (brpel-types--array type))
     ((string= "Value" kind) (brpel-types--value type))
     ((string= "List" kind) (brpel-types--list type))
     ((string= "TupleStruct" kind) (brpel-types--tuple-struct type))
     ((string= "Enum" kind) (brpel-types--enum type))
     ((string= "Struct" kind) (brpel-types--struct type))
     ((string= "Map" kind) (brpel-types--map type))
     ((string= "Set" kind) (brpel-types--set type)))))

(defun brpel-types-inspect (name)
  "Inspect the type in another buffer for type with NAME."
  (with-current-buffer (get-buffer-create "*brpel-type*")
    (erase-buffer)
    (insert (json-encode (brpel-types-from-name name)))
    (goto-char (point-min))
    (json-pretty-print (point-min) (point-max) nil))
  (switch-to-buffer-other-window "*brpel-type*"))

(provide 'brpel-types)
;;; brpel-types.el ends here
