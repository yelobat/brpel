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
;; Homepage: https://github.com/yelobat/brpel-helper
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

(defun brpel-query (&optional components option has with without)
  "Build a `brpel-world-query-synchronously' call.
COMPONENTS is the list of components the entities must have.
OPTION are optional components which entities can have.
HAS is a predicate over optional components, returning true or false.
WITH is a filter, only output entities with these components.
WITHOUT is a filter, don't output entities with these components."
  (brpel-world-query-synchronously
   `((components . ,(vconcat components))
     (option . ,(vconcat option))
     (has . ,(vconcat has)))
   `((with . ,(vconcat with)) (without . ,(vconcat without)))))

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

(cl-defun Sprite (&key size color)
  "Create an alist for a Sprite component with SIZE and COLOR."
  `((,(brpel-type-path "Sprite") .
      ((color . ,(or color (Srgba)))
       (custom_size . ,(or size (Vec2)))))))

(defun Camera2d ()
  "Create an alist for a Camera2d component."
  `((,(brpel-type-path "Camera2d") . ,(make-hash-table))))

(defun brpel-spawn-Camera2d ()
  "Spawn a Camera2d in the world."
  (brpel-world-spawn-entity-synchronously
   `((,(brpel-type-path "Camera2d") . ,(make-hash-table)))))

(defun brpel-clear-color (r g b a)
  "Set the ClearColor resource (assuming srgba) with R, G, B, and A."
  (brpel-world-mutate-resources
   "bevy_camera::clear_color::ClearColor" "0"
   `((Srgba . ((red . ,r)
               (green . ,g)
               (blue . ,b)
               (alpha . ,a))))))

(defun brpel-translate (id &optional x y z)
  "Translate entity with ID by X, Y, and Z."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Transform")
   "translation"
   (vector (or x 0.0) (or y 0.0) (or z 0.0))))

(defun brpel-scale (id &optional w h d)
  "Scale entity with ID by W, H, and D."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Transform")
   "scale"
   (vector (or w 1.0) (or h 1.0) (or d 1.0))))

(provide 'brpel-helper)
;;; brpel-helper.el ends here
