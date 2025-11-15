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
;; Homepage: https://github.com/yelobat/brpel
;; Package-Requires: ((emacs "28.1"))
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

(require 'magit)
(require 'transient)

(defgroup brpel nil
  "Interface with a BRP server."
  :group 'brpel)

(defcustom brpel-remote-url "http://localhost:15702"
  "URL of the BRP running server."
  :type 'string
  :group 'brpel)

(defvar brpel-remote-id 1
  "Incrementing ID for JSON-RPC requests.")

(defvar brpel-component-filters nil
  "The component filters in the ECS browser.")

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

;; BRP Browser
(defconst brpel--browser-buffer "*brpel browser*"
  "The buffer in which the browser is rendered.")

(defvar brpel--current-entity nil
  "The current entity that is being viewed.")

(defvar brpel--current-component nil
  "The current component that is being viewed.")

(defvar brpel--current-resource nil
  "The current resource that is being viewed.")

(defvar brpel--browser-view 'main
  "The current browser view.")

(defconst brpel--temp-file-suffix ".json"
  "The suffix of temp files used to edit components and resources.")

(defconst brpel--temp-file-prefix "brpel"
  "The prefix of temp files used to edit components and resources.")

(defun brpel--browser-refresh-view-and-state ()
  "Refreshes the view state."
  (interactive)
  (setq
   brpel--current-entity nil
   brpel--current-component nil
   brpel--current-resource nil
   brpel--browser-view 'main)
  (brpel--browser-refresh-view))

(define-minor-mode brpel-edit-mode
  "Minor mode for editing components and resources.")

(add-hook 'after-save-hook #'brpel--save-edits)

(defun brpel--save-edits ()
  "Save the edits made to the component or resource to the ECS."
  (if brpel-edit-mode
      (with-current-buffer (current-buffer)
        (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
               (result (json-read-from-string contents)))
          (cond
           (brpel--current-resource (brpel-world-insert-resources brpel--current-resource result))
           (brpel--current-component (brpel-world-insert-components brpel--current-entity result)))))))

(defvar brpel-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "h") #'brpel--browser-menu)
    (define-key map (kbd "RET") #'brpel--browser-select)
    (define-key map (kbd "k") #'brpel--browser-refresh-view-and-state)
    (define-key map (kbd "g") #'brpel--browser-refresh-view)
    map)
  "Keymap for `brpel-browser-mode'.")

(define-derived-mode brpel-browser-mode magit-section-mode "Brpel"
  :group 'brpel
  "Major mode for browsing the Bevy ECS.")

(defun brpel--entities (components)
  "Get all entity IDs which have COMPONENTS."
  (mapcar (lambda (entity) (alist-get 'entity entity))
          (alist-get 'result (brpel-world-query-synchronously
                              `((components . ,(vconcat components))
                                (option . "all")
                                (has . []))
                              `((with . []) (without . []))))))

(defun brpel--try-connection ()
  "Attempts to perform a connection to the BRP server."
  (condition-case _err
        (and (brpel-rpc-discover-synchronously) t)
    (error nil)))

(defun brpel--update-connection ()
  "Update the connection to the BRP server."
  (let ((prompt "Connection failed. Enter BRP server location (CURRENT: %s): "))
    (setq brpel-remote-url
          (read-string (format prompt brpel-remote-url))))
  (message (format "BRP server now located at: '%s'" brpel-remote-url)))

(defun brpel--browser-modeline ()
  "Render the ECS browser's modeline."
  (magit-insert-section (brpel-modeline)
    (insert (format "%-10s" "BRP Server: "))
    (insert (propertize brpel-remote-url 'font-lock-face 'magit-hash))
    (insert "\n\n")))

(defun brpel--browser-resources ()
  "Render the ECS resources in the browser."
  (let* ((result (alist-get 'result (brpel-world-list-resources-synchronously)))
         (names (append result nil))
         (name-count (length names)))
    (magit-insert-section (brpel-resources)
      (magit-insert-heading name-count "Resources")
      (dolist (name names)
        (magit-insert-section (brpel-resource)
          (magit-insert-heading name))))))

(defun brpel--browser-entities ()
  "Render the ECS entities in the browser."
  (let* ((entities (append (brpel--entities brpel-component-filters) nil))
         (entity-count (length entities)))
    (magit-insert-section (brpel-entities)
      (magit-insert-heading entity-count "Entities")
      (dolist (entity (append entities nil))
        (magit-insert-section (brpel-entity)
          (magit-insert-heading (format "%d" entity)))))))

(defun brpel--browser-rpc-methods ()
  "Render the supported RPC methods in the browser."
  (let* ((result (alist-get 'result (brpel-rpc-discover-synchronously)))
         (methods (append (alist-get 'methods result) nil))
         (method-count (length methods)))
    (magit-insert-section (brpel-rpc-methods)
      (magit-insert-heading method-count "RPC Methods")
      (dolist (method methods)
        (magit-insert-section (brpel-rpc-methods)
          (magit-insert-heading (alist-get 'name method)))))))

(defun brpel--browser-component-filters ()
  "Render the current component filters."
  (let ((component-filter-count (length brpel-component-filters)))
    (magit-insert-section (brpel-component-filters)
      (magit-insert-heading component-filter-count "Component Filters")
      (dolist (component-filter brpel-component-filters)
        (magit-insert-section (brpel-component-filter)
          (magit-insert-heading component-filter))))))

(defun brpel--browser-divider ()
  "Insert a dividider in the ECS browser."
  (insert ?\n))

(defun brpel--browser-main-layout ()
  "The main layout of the ECS browser."
  (magit-insert-section (brpel-browser)
    (brpel--browser-modeline)
    (brpel--browser-component-filters)
    (brpel--browser-divider)
    (brpel--browser-resources)
    (brpel--browser-entities)
    (brpel--browser-rpc-methods))
  (magit-section-hide-children (magit-current-section)))

(defun brpel--browser-entity ()
  "Display the current entity in the ECS browser."
  (let* ((result (brpel-world-list-components-synchronously brpel--current-entity))
         (components (append (alist-get 'result result) nil)))
    (magit-insert-section (brpel-entity)
      (magit-insert-heading (format "%d" brpel--current-entity))
      (dolist (component components)
        (magit-insert-section (brpel-component)
          (magit-insert-heading component))))))

(defun brpel--browser-entity-layout ()
  "The entity layout of the ECS browser."
  (magit-insert-section (brpel-entity-browser)
    (brpel--browser-modeline)
    (brpel--browser-divider)
    (brpel--browser-entity)))

(defun brpel--browser-component-view ()
  "The component view of the ECS browser."
  (let* ((file (make-temp-file brpel--temp-file-prefix nil brpel--temp-file-suffix))
         (buffer (find-file-noselect file))
         (result (brpel-world-get-components-synchronously
                  brpel--current-entity
                  (vector brpel--current-component)))
         (component (alist-get 'components (alist-get 'result result))))
    (with-current-buffer buffer
      (brpel-edit-mode)
      (save-excursion
        (insert (json-encode component)))
      (json-pretty-print (point) (point-max))
      (display-buffer-below-selected (current-buffer) nil)
      (switch-to-buffer-other-window (current-buffer)))))

(defun brpel--browser-resource-view ()
  "The resource view of the ECS browser."
  (let* ((file (make-temp-file brpel--temp-file-prefix nil brpel--temp-file-suffix))
         (buffer (find-file-noselect file))
         (result (brpel-world-get-resources-synchronously brpel--current-resource))
         (resource (alist-get 'value (alist-get 'result result))))
    (with-current-buffer buffer
      (brpel-edit-mode)
      (save-excursion
        (insert (json-encode resource)))
      (json-pretty-print (point) (point-max))
      (display-buffer-below-selected (current-buffer) nil)
      (switch-to-buffer-other-window (current-buffer)))))

(defun brpel--browser-layout ()
  "The layout of the ECS browser."
  (cond
   ((equal brpel--browser-view 'main) (brpel--browser-main-layout))
   ((equal brpel--browser-view 'entity) (brpel--browser-entity-layout))))

(defun brpel--browser-render ()
  "Render the ECS browser."
  (interactive)
  (let ((buffer (get-buffer-create brpel--browser-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (brpel-browser-mode)
        (save-excursion
          (brpel--browser-layout))))
    (switch-to-buffer-other-window buffer)))

(defun brpel--browser-refresh-view ()
  "Refresh the ECS browser."
  (interactive)
  (let ((buffer (get-buffer-create brpel--browser-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (brpel-browser-mode)
        (save-excursion
          (brpel--browser-layout))))))

(defun brpel--browser-select ()
  "Select the current section at point."
  (interactive)
  (let* ((type (caar (magit-section-ident (magit-current-section))))
         (title (buffer-substring-no-properties
                 (line-beginning-position)
                 (line-end-position))))
    (cond
     ((equal type 'brpel-entity)
      (setq brpel--browser-view 'entity
            brpel--current-entity (string-to-number title))
      (brpel--browser-refresh-view))
     ((equal type 'brpel-resource)
      (setq brpel--current-resource title)
      (brpel--browser-resource-view))
     ((equal type 'brpel-component)
      (setq brpel--current-component title)
      (brpel--browser-component-view)))))

(brpel-world-get-resources-synchronously "bevy_camera::clear_color::ClearColor")

(defun brpel--browser-add-component-filter ()
  "Accepts a component name to be used as part of the entity filter.
Adds this component name to the list of components used to filter
entities."
  (interactive)
  (let* ((components (alist-get 'result (brpel-world-list-components-synchronously)))
        (collection (append components nil))
        (input (completing-read "Component: " collection)))
    (add-to-list 'brpel-component-filters input))
  (brpel--browser-refresh-view))

(defun brpel--browser-reset-component-filters ()
  "Reset the list of components in the component filter."
  (interactive)
  (setq brpel-component-filters nil)
  (brpel--browser-refresh-view))

(transient-define-prefix brpel--browser-entity-filter-menu ()
  "Entity Component Filter menu."
  ["Actions"
   ("a" "Add filter.." brpel--browser-add-component-filter :transient t)
   ("r" "Reset filters" brpel--browser-reset-component-filters)])

(transient-define-prefix brpel--browser-menu ()
  "ECS browser menu."
  ["Filters"
   ("r" "Select Resource" brpel--browser-entity-filter-menu)
   ("c" "Filter Entities via Components" brpel--browser-entity-filter-menu)])

(defun brpel-browse ()
  "Open the brpel ECS browser."
  (interactive)
  (if (not (brpel--try-connection))
      (brpel--update-connection))
  (condition-case err
      (let nil
        (brpel-rpc-discover-synchronously)
        (brpel--browser-render))
    (error (message (car (last err))))))

(provide 'brpel)
;;; brpel.el ends here
