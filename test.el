;;; test.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 09, 2026
;; Modified: February 09, 2026
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/yelobat/test
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;; This is a showcase of brpel as a method of controlling a Bevy application
;; using Emacs Lisp.
;;
;;; Code:

(require 'brpel)

(defvar test-red-rectangle nil)
(defvar test-white-rectangle nil)
(defvar test-black-rectangle nil)

(defun test-query (&optional components option has with without)
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

(defun test-clear-color (r g b a)
  "Set the ClearColor resource (assuming srgba) with R, G, B, and A."
  (brpel-world-mutate-resources
   "bevy_camera::clear_color::ClearColor" "0"
   `((Srgba . ((red . ,r)
               (green . ,g)
               (blue . ,b)
               (alpha . ,a))))))

(defun test-translate (id &optional x y z)
  "Translate entity with ID by X, Y, and Z."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Transform")
   "translation"
   (vector (or x 0.0) (or y 0.0) (or z 0.0))))

(defun test-scale (id &optional w h d)
  "Scale entity with ID by W, H, and D."
  (brpel-world-mutate-components-synchronously
   id (brpel-type-path "Transform")
   "scale"
   (vector (or w 1.0) (or h 1.0) (or d 1.0))))

(defun test-fetch-entities ()
  "Fetch the red, white and black rectangles."
  (let* ((result (alist-get 'result (test-query (list (brpel-type-path "Name"))))))
    (mapc (lambda (entity) (let* ((components (alist-get 'components entity))
                                  (name (alist-get (intern (brpel-type-path "Name")) components))
                                  (id (alist-get 'entity entity)))
                             (cond
                              ((equal name "Red Rectangle") (setq test-red-rectangle id))
                              ((equal name "White Rectangle") (setq test-white-rectangle id))
                              ((equal name "Black Rectangle") (setq test-black-rectangle id)))
                             nil)) result)))

;; Fetch the red, white and black rectangles.
(test-fetch-entities)

;; Make the background darker.
(test-clear-color 0.05 0.05 0.05 1.0)

;; Reparent the red rectangle and black rectangle to the white rectangle.
(brpel-world-reparent-entities
 (vector test-red-rectangle test-black-rectangle)
 test-white-rectangle)

;; Translate the white rectangle to (0.0, 0.0, 0.0).
(test-translate test-white-rectangle)

;; Translate the black rectangle to (0.0, 0.0, -2.0).
(test-translate test-black-rectangle 0.0 0.0 -2.0)

;; Scale the black rectangle by (1.5, 1.5, 1.0).
(test-scale test-black-rectangle 1.5 1.5)

(brpel-save-scene-synchronously
 "~/brpel/brpel-rs/assets/scenes/test.scn.ron"
 (brpel-map-type-paths (list "Name"
                             "Transform"
                             "GlobalTransform"
                             "Visibility"
                             "InheritedVisibility"
                             "Children"
                             "ChildOf"
                             "ExportableSprite"))
 (brpel-map-type-paths (list "Name")))

(provide 'test)
;;; test.el ends here
