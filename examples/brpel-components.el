;;; brpel-components.el --- Examples of everything relating to components in brpel -*- lexical-binding: t; -*-
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
;;
;;
;;; Code:

(require 'brpel-helper)

;; Spawn a Camera2d
(spawn-entity (list (Camera2d)))

;; Spawn an Entity with a Text2d and Transform component.
(setq text-entity
      (spawn-entity
       (list (Text2d "Hello")
             (Transform :translation (vector 50.0 100.0 0.0)))))

;; Move the text entity back to the center of the screen.
(mutate-translation text-entity)

;; Then move it to the left by 100 pixels.
(mutate-translation text-entity -100.0)

;; Then move it to the right by 100 pixels.
(mutate-translation text-entity 100.0)

;; Then back to the center again
(mutate-translation text-entity)

;; Change the text inside of the Text2d component.
(mutate-Text2d text-entity "What! It changed?")

;; Insert a TextColor component and set it to Red.
(insert-components text-entity (list (TextColor 1.0 0.0 0.0 1.0)))

;; Then insert it again and make it Purple instead.
(insert-components text-entity (list (TextColor 1.0 0.0 1.0 1.0)))

;; Remove the TextColor component from the text entity.
(remove-components text-entity (vector (brpel-type-path "TextColor")))

(provide 'brpel-components)
;;; brpel-components.el ends here
