;;; brpel-spawn.el --- An example showing how to spawn entities from brpel -*- lexical-binding: t; -*-
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
;; In order to test this example, you should first run the Rust example of how
;; to setup brpel usage in your App in brpel-rs. You can run this example by
;; entering that directory and running the following:
;; `cargo run'
;;
;;  Description
;;
;;; Code:

(require 'brpel-helper)

;; Set the request URL of the HTTP server for the Bevy App.
(brpel-request-url-set "http://localhost:3030")

;; Spawn a Camera2d
(spawn-entity (list
               (Camera2d)))

;; Spawn a simple Sprite which is white and has dimensions 50x50.
(spawn-entity
 (list (Sprite :size (vector 50.0 50.0))))

;; Spawn a smaller Sprite which is red and has dimensions 20x20.
;; Place the Sprite at position (50.0, 150.0, 0.0).
(spawn-entity
 (list
  (Sprite :size (vector 20.0 20.0) :color (Srgba :r 1.0 :g 0.0 :b 0.0))
  (Transform :translation (vector 50.0 150.0 0.0))))

;; Spawn 10 Sprites, starting from the center moving towards the top-right
;; corner.
(let ((i 0))
  (while (< i 10)
    (spawn-entity
     (list (Sprite :size (vector 10.0 15.0) :color (Srgba :r 1.0 :g 1.0 :b 0.0))
           (Transform :translation (vector (* i 15.0) (* i 10.0) 0.0))))
    (setq i (1+ i))))

(provide 'brpel-spawn)
;;; brpel-spawn.el ends here
