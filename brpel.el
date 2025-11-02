;;; brpel.el --- Implements the Bevy Remote Protcol (BRP) for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: September 07, 2025
;; Modified: September 30, 2025
;; Version: 0.1.0
;; Keywords: brp comm data docs extensions games hardware lisp local multimedia processes tools unix
;; Homepage: https://github.com/yelobat/brp
;; Package-Requires: ((emacs "28.1") (json-mode "0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Bevy Remote Protocol (BRP) Emacs implementation.
;;
;; # Overview
;;
;;; Code:

(require 'url)
(require 'json)
(require 'json-mode)

(defgroup brpel nil
  "Interface with a BRP server."
  :group 'brpel)

(defcustom brpel-remote-url "http://localhost:15702"
  "URL of the BRP running server."
  :type 'string
  :group 'brpel)

(defvar brpel-remote-id 1
  "Incrementing ID for JSON-RPC requests.")

(defun brpel--default-callback (result)
  "The default callback that simply prints the RESULT of each BRP command."
  (message (json-encode result)))

(defun brpel-send-request (method &optional params callback)
  "Send a JSON-RPC request to the BRP server.

METHOD is the BRP method as a string, see:
https://docs.rs/bevy/latest/bevy/remote/index.html for more details.
PARAMS is an alist representing the JSON object to send as `params'.
CALLBACK is a function to handle the response buffer."

  (let* ((request-id (setq brpel-remote-id (1+ brpel-remote-id)))
         (data (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,request-id)
                     (method . ,method)
                     ,@(when params `((params . ,params))))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (url-retrieve brpel-remote-url
                  (lambda (status)
                    (if-let ((handle-error (plist-get status :error)))
                        (error "Failed to connect to the BRP server"))
                      (when (buffer-live-p (current-buffer))
                        (goto-char url-http-end-of-headers)
                        (let ((json-str (buffer-substring (point) (point-max))))
                          (condition-case err
                              (let ((json-res (json-read-from-string json-str)))
                                (when callback (funcall callback json-res)))
                            (error (message "BRP error: %s" err)))))))
                  nil t))

(defun brpel-send-request-synchronously (method &optional params)
  "Send a synchronous JSON-RPC request to the BRP server.

METHOD is the BRP method as a string, see:
https://docs.rs/bevy/latest/bevy/remote/index.html for more details.
PARAMS is an alist representing the JSON object to send as `params'."

  (let* ((request-id (setq brpel-remote-id (1+ brpel-remote-id)))
         (data (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,request-id)
                     (method . ,method)
                     ,@(when params `((params . ,params))))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously brpel-remote-url t t 1)
      (if-let ((blen (= 0 (buffer-size))))
          (error "Failed to connect to the BRP server"))
      (when (buffer-live-p (current-buffer))
      (goto-char url-http-end-of-headers)
      (let ((json-str (buffer-substring (point) (point-max))))
        (condition-case err
            (let ((json-res (json-read-from-string json-str)))
              json-res)
          (error (message "BRP error: %s" err))))))))

(defun brpel-id (id)
  "Convert an ID in the format <Index>v<Generation> to a u64.
If CALLBACK is non-nil, it will be called on the result of this command."
  (let* ((lst (mapcar #'string-to-number (split-string id "v")))
         (index (nth 0 lst))
         (generation (nth 1 lst)))
    (+ index (* generation (expt 2 32)))))

;; ========= Bevy v0.16.0 =========
;; Method: bevy/get
(defun brpel-get (id components &optional strict callback)
  "Retrieve the values of one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/get"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/query
(defun brpel-query (data filter &optional strict callback)
  "Perform a query over components in the ECS.
Returning all matching entities and their associated component values.
Use DATA, FILTER and STRICT to determine the entities returned.
If CALLBACK is non-nil, it will be called on the result of this command.
See https://docs.rs/bevy_remote/latest/bevy_remote/ for more details."
  (brpel-send-request "bevy/query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/spawn
;; BUG: https://github.com/bevyengine/bevy/issues/20952
;; If you are depending on this function, upgrade to Bevy v0.17.0^
;; and use brpel-world-spawn-entity instead.
(defun brpel-spawn (components &optional callback)
  "Create a new entity with the provided COMPONENTS.
Return the resulting entity ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/spawn"
                      `((components . ,components))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/destroy
(defun brpel-destroy (id &optional callback)
  "Despawn the entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/destroy"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/remove
(defun brpel-remove (id components &optional callback)
  "Delete one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/remove"
                      `((entity . ,id)
                        (components . ,components))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/insert
;; BUG: https://github.com/bevyengine/bevy/issues/20952
;; If you are depending on this function, upgrade to Bevy v0.17.0^
;; and use brpel-world-insert-components
(defun brpel-insert (id components &optional callback)
  "Insert one or more COMPONENTS into an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/insert"
                      `((entity . ,id)
                        (components . ,components))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/mutate_component
(defun brpel-mutate-component (id component path value &optional callback)
  "Mutate a field in a COMPONENT for an entity with ID.
PATH represents the path of the field in the component.
VALUE represents the value to insert at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/mutate_component"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/reparent
(defun brpel-reparent (ids &optional parent callback)
  "Assign a new PARENT to one or more entities for each ID in IDS.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/reparent"
                      `((entities . ,ids)
                        (parent . ,parent))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list
(defun brpel-list (&optional id callback)
  "List all registered components or all components present on an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/list"
                      (if id `((entity . ,id)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/get+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-get+watch (id components &optional strict callback)
  "Watch the values of one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/get+watch"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-list+watch (id &optional callback)
  "Watch all components present on an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/list+watch"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/get_resource
(defun brpel-get-resource (resource-name &optional callback)
  "Extract the value of a given resource with RESOURCE-NAME from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (interactive "sResource name: ")
  (brpel-send-request "bevy/get_resource"
                    `((resource . ,resource-name))
                    (lambda (res)
                      (message (json-encode res)))))

;; Method: bevy/insert_resource
(defun brpel-insert-resource (resource value &optional callback)
  "Extract the VALUE of a given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/insert_resource"
                      `((resource . ,resource)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/remove_resource
(defun brpel-remove-resource (resource &optional callback)
  "Remove the given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/remove_resource"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/mutate_resource
(defun brpel-mutate-resource (resource path value &optional callback)
  "Mutate a field in a RESOURCE.
PATH is the path to the field within the RESOURCE.
VALUE is the value to be inserted at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/mutate_resource"
                      `((resource . ,resource)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list_resources
(defun brpel-list-resources (&optional callback)
  "List all reflectable registered resource types on the BRP server.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "bevy/list_resources" nil
                      (or callback 'brpel--default-callback)))

;; ========= Bevy v0.17.0 =========
;; Method: world.get_components
(defun brpel-world-get-components (id components &optional strict callback)
  "Retrieve the values of one or more COMPONENTS from an entity with a given ID.
Use STRICT to determine whether errors are returned.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.get_components"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-get-components-synchronously (id components &optional strict)
  "Retrieve the values of one or more COMPONENTS from an entity with a given ID.
Use STRICT to determine whether errors are returned."
  (brpel-send-request-synchronously "world.get_components"
                                    `((entity . ,id)
                                      (components . ,components)
                                      (strict . ,(or strict :json-false)))))

;; Method: world.query
(defun brpel-world-query (data filter &optional strict callback)
  "Perform a query over components in the ECS.
Returning all matching entities and their associated component values.
Use DATA, FILTER and STRICT to determine the entities returned.
If CALLBACK is non-nil, it will be called on the result of this command.
See https://docs.rs/bevy_remote/latest/bevy_remote/ for more details."
  (brpel-send-request "world.query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-query-synchronously (data filter &optional strict)
  "Perform a query over components in the ECS.
Returning all matching entities and their associated component values.
Use DATA, FILTER and STRICT to determine the entities returned.
See https://docs.rs/bevy_remote/latest/bevy_remote/ for more details."
  (brpel-send-request-synchronously "world.query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))))

;; Method: world.spawn_entity
(defun brpel-world-spawn-entity (components &optional callback)
  "Create a new entity with the provided COMPONENTS.
This returns the resulting entity ID. If CALLBACK is non-nil,
it will be called on the result of this command."
  (brpel-send-request "world.spawn_entity"
                      `((components . ,components))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-spawn-entity-synchronously (components)
  "Create a new entity with the provided COMPONENTS.
This returns the resulting entity ID."
  (brpel-send-request-synchronously "world.spawn_entity"
                      `((components . ,components))))

;; Method: world.despawn_entity
(defun brpel-world-despawn-entity (id &optional callback)
  "Despawn the entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.despawn_entity"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-despawn-entity-synchronously (id)
  "Despawn the entity with the given ID."
  (brpel-send-request-synchronously "world.despawn_entity"
                                    `((entity . ,id))))

;; Method: world.remove_components
(defun brpel-world-remove-components (id components &optional callback)
  "Delete one or more COMPONENTS from an entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
       (brpel-send-request "world.remove_components"
                           `((entity . ,id)
                             (components . ,components))
                           (or callback 'brpel--default-callback)))

(defun brpel-world-remove-components-synchronously (id components)
  "Delete one or more COMPONENTS from an entity with the given ID."
  (brpel-send-request-synchronously "world.remove_components"
                                    `((entity . ,id)
                                      (components . ,components))))

;; Method: world.insert_components
(defun brpel-world-insert-components (id components &optional callback)
  "Insert one or more COMPONENTS into an entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.insert_components"
                      `((entity . ,id)
                        (components . ,components))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-insert-components-synchronously (id components)
  "Insert one or more COMPONENTS into an entity with the given ID."
  (brpel-send-request-synchronously "world.insert_components"
                                    `((entity . ,id)
                                      (components . ,components))))

;; Method: world.mutate_components
(defun brpel-world-mutate-components (id component path value &optional callback)
  "Mutate a field in a COMPONENT for an entity with a given ID.
PATH is the path to the field within the component.
VALUE is the value to insert at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.mutate_components"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-mutate-components-synchronously (id component path value)
  "Mutate a field in a COMPONENT for an entity with a given ID.
PATH is the path to the field within the component.
VALUE is the value to insert at PATH."
  (brpel-send-request-synchronously "world.mutate_components"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))))

;; Method: world.reparent_entities
(defun brpel-world-reparent-entities (ids &optional parent callback)
  "Assign a new PARENT to one or more entities with an ID in IDS.
If PARENT is non-nil, it will reparent the entity, otherwise it will be
removed from it's current parent.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.reparent_entities"
                      `((entities . ,ids)
                        (parent . ,parent))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-reparent-entities-synchronously (ids &optional parent)
  "Assign a new PARENT to one or more entities with an ID in IDS.
If PARENT is non-nil, it will reparent the entity, otherwise it will be
removed from it's current parent."
  (brpel-send-request-synchronously "world.reparent_entities"
                                    `((entities . ,ids)
                                      (parent . ,parent))))

;; Method: world.list_components
(defun brpel-world-list-components (&optional id callback)
  "List all registered components on an entity with a given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.list_components"
                      (if id `((entity ., id)) nil)
                      (or callback 'brpel--default-callback)))

(defun brpel-world-list-components-synchronously (&optional id)
  "List all registered components on an entity with a given ID."
  (brpel-send-request-synchronously "world.list_components"
                      (if id `((entity ., id)) nil)))

;; Method: world.get_components+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-world-get-components+watch (id components &optional strict callback)
  "Watch the values of one or more COMPONENTS from an entity with a given ID.
if STRICT is t, the result will contain a list of errors that have occurred,
no errors will be retrieved otherwise.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.get_components+watch"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: world.list_components+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-world-list-components+watch (id &optional callback)
  "Watch all components present on an entity with a given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.list_components+watch"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: world.get_resources
(defun brpel-world-get-resources (resource &optional callback)
  "Extract the value of a given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.get_resources"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-get-resources-synchronously (resource)
  "Extract the value of a given RESOURCE from the world."
  (brpel-send-request-synchronously "world.get_resources"
                                    `((resource . ,resource))))

;; Method: world.insert_resources
(defun brpel-world-insert-resources (resource value &optional callback)
  "Insert the given RESOURCE into the world with the given VALUE.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.insert_resources"
                      `((resource . ,resource)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-insert-resources-synchronously (resource value)
  "Insert the given RESOURCE into the world with the given VALUE."
  (brpel-send-request-synchronously "world.insert_resources"
                                    `((resource . ,resource)
                                      (value . ,value))))

;; Method: world.remove_resources
(defun brpel-world-remove-resources (resource &optional callback)
  "Remove the given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.remove_resources"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-remove-resources-synchronously (resource)
  "Remove the given RESOURCE from the world."
  (brpel-send-request-synchronously "world.remove_resources"
                      `((resource . ,resource))))

;; Method: world.mutate_resources
(defun brpel-world-mutate-resources (resource path value &optional callback)
  "Mutate a field in a RESOURCE with PATH and set it to VALUE.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.mutate_resources"
                      `((resource . ,resource)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-mutate-resources-synchronously (resource path value)
  "Mutate a field in a RESOURCE with PATH and set it to VALUE."
  (brpel-send-request-synchronously "world.mutate_resources"
                                    `((resource . ,resource)
                                      (path . ,path)
                                      (value . ,value))))

;; Method: world.list_resources
(defun brpel-world-list-resources (&optional callback)
  "List all reflectable registered resource types.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "world.list_resources" nil
                      (or callback 'brpel--default-callback)))

(defun brpel-world-list-resources-synchronously ()
  "List all reflectable registered resource types."
  (brpel-send-request-synchronously "world.list_resources" nil))

;; Method: registry.schema
(defun brpel-registry-schema (&optional with-crates without-crates type-limit callback)
  "Retrieve schema information about registered types in the current app.
WITH-CRATES is an array of crate names to include in the result.
WITHOUT-CRATES is an array of crate names to exclude from the results.
TYPE-LIMIT contains an array of with and without
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "registry.schema"
                      (if (and with-crates without-crates type-limit)
                          `((with_crates . ,with-crates)
                            (without_crates . ,without-crates)
                            (type_limit . ,type-limit))
                        nil)
                      (or callback 'brpel--default-callback)))

(defun brpel-registry-schema-synchronously (&optional with-crates without-crates type-limit)
  "Retrieve schema information about registered types in the current app.
WITH-CRATES is an array of crate names to include in the result.
WITHOUT-CRATES is an array of crate names to exclude from the results.
TYPE-LIMIT contains an array of with and without."
  (brpel-send-request-synchronously "registry.schema"
                      (if (and with-crates without-crates type-limit)
                          `((with_crates . ,with-crates)
                            (without_crates . ,without-crates)
                            (type_limit . ,type-limit))
                        nil)))

;; Method: rpc.discover
(defun brpel-rpc-discover (&optional callback)
  "Discover available remote methods and server information.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-send-request "rpc.discover" nil
                      (or callback 'brpel--default-callback)))

(defun brpel-rpc-discover-synchronously ()
  "Discover available remote methods and server information."
  (brpel-send-request-synchronously "rpc.discover" nil))


;; brpel browser
(defvar brpel--browser-top-directories
  '("Resources" "Entities" "RPC_Methods")
  "Fake directories shown at the root of the browser.")

(defvar brpel--browser-current-path nil
  "The current browser path.")

(defconst brpel--browser-buffer "brpel browser")

(defun brpel--browser-insert-root ()
  "Insert the top-level directories into the current buffer."
  (dolist (name brpel--browser-top-directories)
    (insert-button name
                   'action (lambda (_) (brpel--browser-render (list name)))
                   'follow-link nil
                   'face 'dired-directory)
    (insert "\n")))

(defvar brpel-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'brpel--browser-open)
    (define-key map (kbd "^") #'brpel--browser-up)
    (define-key map (kbd "C-c C-k") #'brpel--browser-up)
    (define-key map (kbd "C-c C-g") #'brpel--browser-render)
    (define-key map (kbd "C-c C-q") #'quit-window)
    map)
  "Keymap for `brpel-browser-mode'.")

(defvar brpel-json-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'brpel--browser-up)
    (define-key map (kbd "C-c C-s") #'brpel--browser-save)
    (define-key map (kbd "C-c C-q") #'quit-window)
    map)
  "Keymap for `brpel-json-mode'.")

(define-derived-mode brpel-json-mode json-mode "brpel-json"
  "Major mode for editing Resources and Components."
  (buffer-enable-undo)
  (setq buffer-read-only nil))

(define-derived-mode brpel-browser-mode special-mode "brpel-browser"
  "Major mode for browsing the Bevy ECS via a dired-like interface."
  (buffer-disable-undo)
  (setq buffer-read-only t))

;; TODO The component update is slow, should only
;; update the paths in the component that were changed.
(defun brpel--browser-save ()
  "Save the contents and sent to the Bevy ECS."
  (interactive)
  (cond
   ((equal (car brpel--browser-current-path) "Entities")
    (let* ((content (split-string (with-current-buffer (get-buffer-create "brpel browser")
                                    (buffer-substring-no-properties (point-min) (point-max))) "\n"))
           (entity (string-to-number (elt brpel--browser-current-path 1)))
           (component (elt brpel--browser-current-path 2))
           (values (cdr (car (json-read-from-string (mapconcat #'concat (cdr content) "\n"))))))
      (dolist (value values)
        (let ((path (symbol-name (car value)))
              (new-value (cdr value)))
          (brpel-world-mutate-components-synchronously entity component path new-value)))))
   ((equal (car brpel--browser-current-path) "Resources")
    (let* ((content (split-string (with-current-buffer (get-buffer-create "brpel browser")
                                    (buffer-substring-no-properties (point-min) (point-max))) "\n"))
           (resource (car (last (split-string (car content) "/"))))
           (value (json-read-from-string (mapconcat #'concat (cdr content) "\n"))))
      (brpel-world-insert-resources-synchronously resource value)))))

(defun brpel--browser-open ()
  "Open the directory or file at point."
  (call-interactively #'push-button))

(defun brpel--browser-up ()
  "Move up in the directory hierarchy in the browser."
  (interactive)
  (when brpel--browser-current-path
    (brpel--browser-render (butlast brpel--browser-current-path))))

(defun brpel--browser-insert-json-buffer (data)
  "Insert DATA (alist) as pretty-printed JSON data."
  (let ((inhibit-read-only t))
    (brpel-json-mode)
    (save-excursion
      (insert (json-encode data)))
    (json-pretty-print (point) (point-max))))

(defun brpel--browser-insert-current-path ()
  "Insert the current browser path into the current buffer."
  (let* ((remote (concat brpel-remote-url "/"))
         (path (mapconcat #'concat brpel--browser-current-path "/"))
         (time (format-time-string "%b %e %H:%M")))
    (insert (format "%s\n" (concat remote path)))))

(defun brpel--browser-insert-resources (path)
  "List all reflectable resources.
PATH contains Resources followed by an optional Resource Name."
  (cond
   ((= (length path) 1)
    (let ((names (append (alist-get 'result (brpel-world-list-resources-synchronously)) nil)))
      (dolist (name names)
        (insert-button name
                       'action (lambda (_) (brpel--browser-render (append path (list name))))
                       'follow-link nil
                       'face 'dired-directory)
        (insert "\n"))))
   ((= (length path) 2)
    (let* ((resource (elt path 1))
           (data (brpel-world-get-resources-synchronously resource))
           (json (alist-get 'value (alist-get 'result data))))
      (brpel--browser-insert-json-buffer json)))))

(defun brpel--browser-insert-rpc-methods ()
  "Insert all of the supported RPC Methods by the current server."
  (let* ((results (alist-get 'result (brpel-rpc-discover-synchronously)))
         (methods (append (alist-get 'methods results) nil)))
    (dolist (method methods)
      (insert (format "%s\n"(alist-get 'name method))))))

(defun brpel--all-entities ()
  "Get all the entity IDs from the current bevy server."
  (mapcar (lambda (entity) (alist-get 'entity entity))
          (alist-get 'result (brpel-world-query-synchronously
                              `((components . [])
                                (option . "all")
                                (has . []))
                              `((with . []) (without . []))))))

(defun brpel--browser-insert-entities (path)
  "Insert all Entities currently present in the Bevy app.
PATH contains Entities followed by an optional Entity ID, and Component Name."
  (cond
   ((= (length path) 1)
    (let ((entities (brpel--all-entities)))
      (dolist (entity entities)
        (insert-button (format "%d\n" entity)
                       'action (lambda (_) (brpel--browser-render (append path (list (format "%d" entity)))))
                       'follow-link nil
                       'face 'dired-directory))))
   ((= (length path) 2)
    (let* ((entity (string-to-number (elt path 1)))
           (components (append (alist-get 'result (brpel-world-list-components-synchronously entity)) nil)))
      (dolist (component components)
        (insert-button (format "%s\n" component)
                       'action (lambda (_) (brpel--browser-render (append path (list component))))
                       'follow-link nil
                       'face 'dired-directory))))
   ((= (length path) 3)
    (let* ((entity (string-to-number (elt path 1)))
           (component (elt path 2))
           (data (brpel-world-get-components-synchronously
                  entity
                  (vector component)))
           (json (alist-get 'components (alist-get 'result data))))
      (brpel--browser-insert-json-buffer json)))))

(defun brpel--browser-render (&optional path)
  "Render the browser buffer for PATH."
  (interactive)
  (with-current-buffer (get-buffer-create brpel--browser-buffer)
    (brpel-browser-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq brpel--browser-current-path path)
      (brpel--browser-insert-current-path)
      (save-excursion
        (cond
         ((null path) (brpel--browser-insert-root))
         ((equal (car path) "Resources") (brpel--browser-insert-resources path))
         ((equal (car path) "Entities") (brpel--browser-insert-entities path))
         ((equal (car path) "RPC_Methods") (brpel--browser-insert-rpc-methods)))))))

(defun brpel-browse ()
  "Open the brpel ECS browser."
  (interactive)
  (condition-case err
      (let nil
        (brpel-rpc-discover-synchronously)
        (switch-to-buffer brpel--browser-buffer)
        (brpel--browser-render nil))
    (error (message (car (last err))))))

(provide 'brpel)
;;; brpel.el ends here
