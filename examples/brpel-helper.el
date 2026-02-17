;;; brpel-helper.el --- A file providing helper functions for brpel -*- lexical-binding: t; -*-
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
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'brpel)

(defun spawn-entity (components)
  "Spawn an entity with COMPONENTS."
  (let* ((response (brpel-world-spawn-entity-synchronously
                    (apply #'append components)))
         (result (alist-get 'result response))
         (entity (alist-get 'entity result)))
        entity))

(cl-defun Srgba (&key r g b a)
  "Return an Srgba list with R, G, B, and A."
  (let ((red (or r 1.0))
        (green (or g 1.0))
        (blue (or b 1.0))
        (alpha (or a 1.0)))
    `((Srgba . ((red . ,red) (green . ,green) (blue . ,blue) (alpha . ,alpha))))))

(defun Text2d (string)
  "Create an alist for a Text2d component with STRING."
  `((,(brpel-type-path "Text2d") . ,string)))

(defun TextColor (r g b a)
  "Create an alist for a TextColor component with R, G, B, and A."
  `((,(brpel-type-path "TextColor") . ,(Srgba :r r :g g :b b :a a))))

(cl-defun Vec2 (&key x y)
  "Return a Vec2 list with X and Y."
  (vector (or x 0.0) (or y 0.0)))

(cl-defun Transform (&key translation rotation scale)
  "Create an alist for a Transform component with TRANSLATION, ROTATION and SCALE."
  (let ((translation (or translation (vector 0.0 0.0 0.0)))
        (rotation (or rotation (vector 0.0 0.0 0.0 1.0)))
        (scale (or scale (vector 1.0 1.0 1.0))))
    `((,(brpel-type-path "Transform") .
       (("translation" . ,translation)
        ("rotation" . ,rotation)
        ("scale" . ,scale))))))

;; NOTE This is just a simple example of how to spawn a Sprite.
;; Loading images is not supported as serialization of Handles
;; is not supported by Bevy as of version 0.18.0.
(cl-defun Sprite (&key size color)
  "Create an alist for a Sprite component with SIZE and COLOR."
  `((,(brpel-type-path "Sprite") .
      ((color . ,(or color (Srgba)))
       (custom_size . ,(or size (Vec2)))))))

(defun Camera2d ()
  "Create an alist for a Camera2d component."
  `((,(brpel-type-path "Camera2d") . ,(make-hash-table))))

(defun mutate-translation (id &optional x y z)
  "Mutate the translation field on a Transform component with X, Y, and Z.
This mutation is applied to the entity with ID."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Transform")
   "translation"
   (vector (or x 0.0) (or y 0.0) (or z 0.0))))

(defun mutate-Text2d (id &optional string)
  "Mutate the string of a Text2d component with STRING.
This mutation is applied to the entity with ID."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Text2d") "0"
   string))

(defun insert-components (id components)
  "Insert one or more COMPONENTS into entity with ID."
  (brpel-world-insert-components-synchronously id (apply #'append components)))

(defun remove-components (id components)
  "Remove one or more COMPONENTS from an entity with ID."
  (brpel-world-remove-components-synchronously id components))

(defun mutate-resource (resource path value)
  "Mutate the RESOURCE's VALUE of the field at PATH."
  (brpel-world-mutate-resources-synchronously resource path value))

(defun insert-resource (resource value)
  "Insert VALUE into RESOURCE."
  (brpel-world-insert-resources-synchronously resource value))

(provide 'brpel-helper)
;;; brpel-helper.el ends here
