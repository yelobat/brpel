;;; brpel-browser.el --- The brpel browser -*- lexical-binding: t; -*-
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

(require 'brpel-common)
(require 'brpel-request)
(require 'brpel-methods)

(require 'magit)
(require 'json)

(defconst brpel-browser--buffer "*brpel browser*"
  "The buffer in which the browser is rendered.")

(defvar brpel-browser--current-entity nil
  "The current entity that is being viewed.")

(defvar brpel-browser--current-component nil
  "The current component that is being viewed.")

(defvar brpel-browser--current-resource nil
  "The current resource that is being viewed.")

(defvar brpel-browser--current-view 'main
  "The current browser view.")

(defconst brpel-browser--temp-file-suffix ".json"
  "The suffix of temp files used to edit components and resources.")

(defconst brpel-browser--temp-file-prefix "brpel"
  "The prefix of temp files used to edit components and resources.")

(defvar brpel-browser--component-filters nil
  "The component filters in the ECS browser.")

(defvar brpel-browser--marked-children nil
  "The current list marked children.")

(defvar brpel-browser--marked-parent nil
  "The current marked parent.")

(defun brpel-browser--refresh-view-and-state ()
  "Refreshes the view state."
  (interactive)
  (setq
   brpel-browser--current-entity nil
   brpel-browser--current-component nil
   brpel-browser--current-resource nil
   brpel-browser--current-view 'main)
  (brpel-browser--refresh-view))

(define-minor-mode brpel-browser-edit-mode
  "Minor mode for editing components and resources.")

(add-hook 'after-save-hook #'brpel-browser--save-edits)

(defun brpel-browser--save-edits ()
  "Save the edits made to the component or resource to the ECS."
  (if brpel-edit-mode
      (with-current-buffer (current-buffer)
        (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
               (result (json-read-from-string contents)))
          (cond
           (brpel-browser--current-resource
            (brpel-world-insert-resources brpel-browser--current-resource result))
           (brpel-browser--current-component
            (brpel-world-insert-components brpel-browser--current-entity result)))))))

(defvar brpel-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "h") #'brpel-browser--menu)
    (define-key map (kbd "RET") #'brpel-browser--select)
    (define-key map (kbd "DEL") #'brpel-browser--remove-component-filter)
    (define-key map (kbd "k") #'brpel-browser--refresh-view-and-state)
    (define-key map (kbd "g") #'brpel-browser--refresh-view)
    map)
  "Keymap for `brpel-browser-mode'.")

(define-derived-mode brpel-browser-mode magit-section-mode "Brpel"
  :group 'brpel
  "Major mode for browsing the Bevy ECS.")

(defun brpel-browser--entities (components)
  "Get the entity IDs for all entities that have COMPONENTS.
If COMPONENTS is empty, return a hierarchical structure.
If COMPONENTS is non-empty, return a flat structure."
  (if (> (length components) 0)
      (brpel-browser--entities-flat components)
    (brpel-browser--entities-hierarchy)))

(defun brpel-browser--entities-flat (components)
  "Get the entity IDs for all entities that have COMPONENTS."
  (let* ((name-type-path (brpel-type-path "Name"))
         (response (brpel-world-query-synchronously
                    `((components . ,(vconcat components))
                      (option . ,(vector name-type-path))
                      (has . []))
                    `((with . []) (without . []))))
         (result (append (alist-get 'result response) nil)))
    (mapcar (lambda (item) (let* ((entity (alist-get 'entity item))
                                  (name (alist-get (intern name-type-path) (alist-get 'components item))))
                             (list entity name nil))) result)))

(defun brpel-browser--entities-hierarchy ()
  "Get the entity hierarchy for all entities."
  (let* ((name-type-path (brpel-type-path "Name"))
         (children-type-path (brpel-type-path "Children"))
         (response (brpel-world-query-synchronously
                    `((components . [])
                      (option . ,(vector name-type-path children-type-path))
                      (has . []))
                    `((with . []) (without . []))))
         (result (append (alist-get 'result response) nil))
         (entity-map (make-hash-table :test 'equal))
         (all-children-ids (make-hash-table :test 'equal))
         (children-key (intern children-type-path))
         (name-key (intern name-type-path)))

    (dolist (item result)
      (let* ((id (alist-get 'entity item))
             (components (alist-get 'components item))
             (name (alist-get name-key components))
             (child-ids (append (alist-get children-key components) nil)))
        (puthash id (list id name child-ids) entity-map)
        (dolist (c-id child-ids)
          (puthash c-id t all-children-ids))))

    (cl-labels ((build-tree (id)
                  (let* ((entry (gethash id entity-map))
                         (name (cadr entry))
                         (c-ids (caddr entry)))
                    (list id name (mapcar #'build-tree c-ids)))))
      (let (hierarchy)
        (maphash (lambda (id _data)
                   (unless (gethash id all-children-ids)
                     (push (build-tree id) hierarchy)))
                 entity-map)
        hierarchy))))

(defun brpel-browser--try-connection ()
  "Attempts to perform a connection to the BRP server."
  (condition-case _err
        (and (brpel-rpc-discover-synchronously) t)
    (error nil)))

(defun brpel-browser--update-connection ()
  "Update the connection to the BRP server."
  (let ((prompt "Connection failed. Enter BRP server location (CURRENT: %s): "))
    (brpel-request-url-set (read-string (format prompt brpel-remote-url))))
  (message (format "BRP server now located at: '%s'" brpel-remote-url)))

(defun brpel-browser--modeline ()
  "Render the ECS browser's modeline."
  (magit-insert-section (brpel-modeline)
    (insert (format "%-10s" "BRP Server: "))
    (insert (propertize brpel-remote-url 'font-lock-face 'magit-hash))
    (insert "\n\n")))

(defun brpel-browser--resources-view ()
  "Render the ECS resources in the browser."
  (let* ((result (alist-get 'result (brpel-world-list-resources-synchronously)))
         (names (append result nil))
         (name-count (length names)))
    (magit-section-hide-children (magit-insert-section (brpel-resources)
      (magit-insert-heading name-count "Resources")
      (dolist (name names)
        (magit-insert-section (brpel-resource (list name))
          (magit-insert-heading name)))))))

(defun brpel-browser--entities-view ()
  "Render the ECS entities in the browser."
  (let* ((entities (brpel-browser--entities brpel-browser--component-filters))
         (entity-count (length entities)))
    (magit-section-hide-children (magit-insert-section (brpel-entities)
      (magit-insert-heading entity-count "Entities")
      (dolist (entity entities)
        (brpel-browser--insert-entity-subtree entity 0))))))

(defun brpel-browser--insert-entity-subtree (subtree depth)
  "Insert SUBTREE into the brpel browser.
DEPTH is used internally to format the data."
  (pcase-let ((`(,entity ,name ,children) subtree))
    (let ((indent (concat (make-string (* 2 depth) ?-) "+")))
    (magit-section-hide-children (magit-insert-section (brpel-entity (cons entity name))
      (magit-insert-heading (format "%s %s" indent (or name entity)))
      (dolist (child-tree children)
        (brpel-browser--insert-entity-subtree child-tree (1+ depth))))))))

(defun brpel-browser--entity-name (entity)
  "Return the Name component of ENTITY. Return nil if no such component exists."
  (let* ((name-type-path (brpel-type-path "Name"))
         (response (brpel-world-get-components-synchronously entity (list name-type-path)))
         (result (alist-get 'result response))
         (components (alist-get 'components result))
         (name (alist-get (intern name-type-path) components)))
    name))

(defun brpel-browser--rpc-methods-view ()
  "Render the supported RPC methods in the browser."
  (let* ((result (alist-get 'result (brpel-rpc-discover-synchronously)))
         (methods (append (alist-get 'methods result) nil))
         (method-count (length methods)))
    (magit-insert-section (brpel-rpc-methods)
      (magit-insert-heading method-count "RPC Methods")
      (dolist (method methods)
        (magit-insert-section (brpel-rpc-methods)
          (magit-insert-heading (alist-get 'name method)))))))

(defun brpel-browser--component-filters-view ()
  "Render the current component filters."
  (let ((component-filter-count (length brpel-browser--component-filters)))
    (magit-insert-section (brpel-component-filters)
      (magit-insert-heading component-filter-count "Component Filters")
      (dolist (component-filter brpel-browser--component-filters)
        (magit-insert-section (brpel-component-filter)
          (magit-insert-heading component-filter))))))

(defun brpel-browser--divider ()
  "Insert a dividider in the ECS browser."
  (insert ?\n))

(defun brpel-browser--main-view ()
  "The main view of the ECS browser."
  (magit-section-hide-children
   (magit-insert-section (brpel-browser)
     (brpel-browser--modeline)
     (brpel-browser--component-filters-view)
     (brpel-browser--divider)
     (brpel-browser--resources-view)
     (brpel-browser--entities-view)
     (brpel-browser--rpc-methods-view))))

(defun brpel-browser--entity ()
  "Display the current entity in the ECS browser."
  (let* ((result (brpel-world-list-components-synchronously brpel-browser--current-entity))
         (components (append (alist-get 'result result) nil)))
    (magit-insert-section (brpel-entity)
      (magit-insert-heading (format "%d" brpel-browser--current-entity))
      (dolist (component components)
        (magit-insert-section (brpel-component (list component))
          (magit-insert-heading component))))))

(defun brpel-browser--entity-view ()
  "The entity view of the ECS browser."
  (magit-insert-section (brpel-entity-browser)
    (brpel-browser--modeline)
    (brpel-browser--divider)
    (brpel-browser--entity)))

(defun brpel-browser--component-view ()
  "The component view of the ECS browser."
  (let* ((file (make-temp-file brpel-browser--temp-file-prefix nil brpel-browser--temp-file-suffix))
         (buffer (find-file-noselect file))
         (result (brpel-world-get-components-synchronously
                  brpel-browser--current-entity
                  (vector brpel-browser--current-component)))
         (component (alist-get 'components (alist-get 'result result))))
    (with-current-buffer buffer
      (brpel-browser-edit-mode)
      (save-excursion
        (insert (json-encode component)))
      (json-pretty-print (point) (point-max))
      (switch-to-buffer-other-window (current-buffer)))))

(defun brpel-browser--resource-view ()
  "The resource view of the ECS browser."
  (let* ((file (make-temp-file brpel-browser--temp-file-prefix nil brpel-browser--temp-file-suffix))
         (buffer (find-file-noselect file))
         (result (brpel-world-get-resources-synchronously brpel-browser--current-resource))
         (resource (alist-get 'value (alist-get 'result result))))
    (with-current-buffer buffer
      (brpel-browser-edit-mode)
      (save-excursion
        (insert (json-encode resource)))
      (json-pretty-print (point) (point-max))
      (switch-to-buffer-other-window (current-buffer)))))

(defun brpel-browser--view ()
  "The view of the ECS browser."
  (cond
   ((equal brpel-browser--current-view 'main) (brpel-browser--main-view))
   ((equal brpel-browser--current-view 'entity) (brpel-browser--entity-view))))

(defun brpel-browser--render ()
  "Render the ECS browser."
  (interactive)
  (let ((buffer (get-buffer-create brpel-browser--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (brpel-browser-mode)
        (save-excursion
          (brpel-browser--view))))
    (switch-to-buffer buffer)))

(defun brpel-browser--refresh-view ()
  "Refresh the ECS browser."
  (interactive)
  (let ((buffer (get-buffer-create brpel-browser--buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (brpel-browser-mode)
        (save-excursion
          (brpel-browser--view))))))

(defun brpel-browser--select ()
  "Select the current section at point."
  (interactive)
  (let* ((ident (magit-section-ident (magit-current-section)))
         (type (caar ident))
         (value (cdar ident))
         (id (car value)))
    (message "%s" type)
    (cond
     ((equal type 'brpel-entity)
      (setq brpel-browser--view 'entity
            brpel-browser--current-entity id)
      (brpel-browser--refresh-view))
     ((equal type 'brpel-resource)
      (setq brpel-browser--current-resource id)
      (brpel-browser--resource-view))
     ((equal type 'brpel-component)
      (setq brpel-browser--current-component id)
      (brpel-browser--component-view)))))

(defun brpel-browser--remove-component-filter ()
  "Remove the current component filter at point."
  (interactive)
  (let* ((type (caar (magit-section-ident (magit-current-section))))
         (title (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
    (if (equal type 'brpel-component-filter)
        (let nil
          (setq brpel-browser--component-filters
                (remove title brpel-browser--component-filters))
          (brpel-browser--refresh-view)))))

(defun brpel-browser--component-filter-add ()
  "Accepts a component name to be used as part of the entity filter.
Adds this component name to the list of components used to filter
entities."
  (interactive)
  (let* ((components (alist-get 'result (brpel-world-list-components-synchronously)))
        (collection (append components nil))
        (input (completing-read "Component: " collection)))
    (add-to-list 'brpel-browser--component-filters input))
  (brpel-browser--refresh-view))

(defun brpel-browser--component-filters-reset ()
  "Reset the list of components in the component filter."
  (interactive)
  (setq brpel-browser--component-filters nil)
  (brpel-browser--refresh-view))

(transient-define-prefix brpel-browser--entity-filter-menu ()
  "Entity Component Filter menu."
  ["Actions"
   ("a" "Add filter.." brpel-browser--component-filter-add :transient t)
   ("r" "Reset filters" brpel-browser--component-filters-reset)])

(transient-define-prefix brpel-browser--menu ()
  "ECS browser menu."
  ["Filters"
   ("c" "Filter Entities via Components" brpel-browser--entity-filter-menu)]

  ["Entities"
   ("r" "Reparent marked children under marked parent.")])

(defun brpel-browse ()
  "Open the brpel ECS browser."
  (interactive)
  (if (not (brpel-browser--try-connection))
      (brpel-browser--update-connection))
  (condition-case err
      (progn
        (brpel--registry-schema-index-populate)
        (brpel-browser--render))
    (error (message err))))

(provide 'brpel-browser)
;;; brpel-browser.el ends here
