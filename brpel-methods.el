;;; brpel-methods.el --- The methods support by brpel for BRP -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: February 12, 2026
;; Modified: February 12, 2026
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

(require 'brpel-common)
(require 'brpel-request)

;; ========= Bevy v0.16.0 =========
;; Method: bevy/get
(defun brpel-get (id components &optional strict callback)
  "Retrieve the values of one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/get"
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
  (brpel-request-send "bevy/query"
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
  (brpel-request-send "bevy/spawn"
                      `((components . ,components))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/destroy
(defun brpel-destroy (id &optional callback)
  "Despawn the entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/destroy"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/remove
(defun brpel-remove (id components &optional callback)
  "Delete one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/remove"
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
  (brpel-request-send "bevy/insert"
                      `((entity . ,id)
                        (components . ,components))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/mutate_component
(defun brpel-mutate-component (id component path value &optional callback)
  "Mutate a field in a COMPONENT for an entity with ID.
PATH represents the path of the field in the component.
VALUE represents the value to insert at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/mutate_component"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/reparent
(defun brpel-reparent (ids &optional parent callback)
  "Assign a new PARENT to one or more entities for each ID in IDS.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/reparent"
                      `((entities . ,ids)
                        (parent . ,parent))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list
(defun brpel-list (&optional id callback)
  "List all registered components or all components present on an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/list"
                      (if id `((entity . ,id)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/get+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-get+watch (id components &optional strict callback)
  "Watch the values of one or more COMPONENTS from an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/get+watch"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-list+watch (id &optional callback)
  "Watch all components present on an entity with ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/list+watch"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/get_resource
(defun brpel-get-resource (resource-name &optional callback)
  "Extract the value of a given resource with RESOURCE-NAME from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (interactive "sResource name: ")
  (brpel-request-send "bevy/get_resource"
                    `((resource . ,resource-name))
                    (or callback 'brpel--default-callback)))

;; Method: bevy/insert_resource
(defun brpel-insert-resource (resource value &optional callback)
  "Extract the VALUE of a given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/insert_resource"
                      `((resource . ,resource)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/remove_resource
(defun brpel-remove-resource (resource &optional callback)
  "Remove the given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/remove_resource"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/mutate_resource
(defun brpel-mutate-resource (resource path value &optional callback)
  "Mutate a field in a RESOURCE.
PATH is the path to the field within the RESOURCE.
VALUE is the value to be inserted at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/mutate_resource"
                      `((resource . ,resource)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

;; Method: bevy/list_resources
(defun brpel-list-resources (&optional callback)
  "List all reflectable registered resource types on the BRP server.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "bevy/list_resources" nil
                      (or callback 'brpel--default-callback)))

;; ========= Bevy v0.17.0 =========
;; Method: world.get_components
(defun brpel-world-get-components (id components &optional strict callback)
  "Retrieve the values of one or more COMPONENTS from an entity with a given ID.
Use STRICT to determine whether errors are returned.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.get_components"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-get-components-synchronously (id components &optional strict)
  "Retrieve the values of one or more COMPONENTS from an entity with a given ID.
Use STRICT to determine whether errors are returned."
  (brpel-request-send-synchronously "world.get_components"
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
  (brpel-request-send "world.query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-query-synchronously (data filter &optional strict)
  "Perform a query over components in the ECS.
Returning all matching entities and their associated component values.
Use DATA, FILTER and STRICT to determine the entities returned.
See https://docs.rs/bevy_remote/latest/bevy_remote/ for more details."
  (brpel-request-send-synchronously "world.query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))))

;; Method: world.spawn_entity
(defun brpel-world-spawn-entity (components &optional callback)
  "Create a new entity with the provided COMPONENTS.
This returns the resulting entity ID. If CALLBACK is non-nil,
it will be called on the result of this command."
  (brpel-request-send "world.spawn_entity"
                      `((components . ,components))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-spawn-entity-synchronously (components)
  "Create a new entity with the provided COMPONENTS.
This returns the resulting entity ID."
  (brpel-request-send-synchronously "world.spawn_entity"
                      `((components . ,components))))

;; Method: world.despawn_entity
(defun brpel-world-despawn-entity (id &optional callback)
  "Despawn the entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.despawn_entity"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-despawn-entity-synchronously (id)
  "Despawn the entity with the given ID."
  (brpel-request-send-synchronously "world.despawn_entity"
                                    `((entity . ,id))))

;; Method: world.remove_components
(defun brpel-world-remove-components (id components &optional callback)
  "Delete one or more COMPONENTS from an entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
       (brpel-request-send "world.remove_components"
                           `((entity . ,id)
                             (components . ,components))
                           (or callback 'brpel--default-callback)))

(defun brpel-world-remove-components-synchronously (id components)
  "Delete one or more COMPONENTS from an entity with the given ID."
  (brpel-request-send-synchronously "world.remove_components"
                                    `((entity . ,id)
                                      (components . ,components))))

;; Method: world.insert_components
(defun brpel-world-insert-components (id components &optional callback)
  "Insert one or more COMPONENTS into an entity with the given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.insert_components"
                      `((entity . ,id)
                        (components . ,components))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-insert-components-synchronously (id components)
  "Insert one or more COMPONENTS into an entity with the given ID."
  (brpel-request-send-synchronously "world.insert_components"
                                    `((entity . ,id)
                                      (components . ,components))))

;; Method: world.mutate_components
(defun brpel-world-mutate-components (id component path value &optional callback)
  "Mutate a field in a COMPONENT for an entity with a given ID.
PATH is the path to the field within the component.
VALUE is the value to insert at PATH.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.mutate_components"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-mutate-components-synchronously (id component path value)
  "Mutate a field in a COMPONENT for an entity with a given ID.
PATH is the path to the field within the component.
VALUE is the value to insert at PATH."
  (brpel-request-send-synchronously "world.mutate_components"
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
  (brpel-request-send "world.reparent_entities"
                      `((entities . ,ids)
                        (parent . ,parent))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-reparent-entities-synchronously (ids &optional parent)
  "Assign a new PARENT to one or more entities with an ID in IDS.
If PARENT is non-nil, it will reparent the entity, otherwise it will be
removed from it's current parent."
  (brpel-request-send-synchronously "world.reparent_entities"
                                    `((entities . ,ids)
                                      (parent . ,parent))))

;; Method: world.list_components
(defun brpel-world-list-components (&optional id callback)
  "List all registered components on an entity with a given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.list_components"
                      (if id `((entity ., id)) nil)
                      (or callback 'brpel--default-callback)))

(defun brpel-world-list-components-synchronously (&optional id)
  "List all registered components on an entity with a given ID."
  (brpel-request-send-synchronously "world.list_components"
                      (if id `((entity ., id)) nil)))

;; Method: world.get_components+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-world-get-components+watch (id components &optional strict callback)
  "Watch the values of one or more COMPONENTS from an entity with a given ID.
if STRICT is t, the result will contain a list of errors that have occurred,
no errors will be retrieved otherwise.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.get_components+watch"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (or callback 'brpel--default-callback)))

;; Method: world.list_components+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-world-list-components+watch (id &optional callback)
  "Watch all components present on an entity with a given ID.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.list_components+watch"
                      `((entity . ,id))
                      (or callback 'brpel--default-callback)))

;; Method: world.get_resources
(defun brpel-world-get-resources (resource &optional callback)
  "Extract the value of a given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.get_resources"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-get-resources-synchronously (resource)
  "Extract the value of a given RESOURCE from the world."
  (brpel-request-send-synchronously "world.get_resources"
                                    `((resource . ,resource))))

;; Method: world.insert_resources
(defun brpel-world-insert-resources (resource value &optional callback)
  "Insert the given RESOURCE into the world with the given VALUE.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.insert_resources"
                      `((resource . ,resource)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-insert-resources-synchronously (resource value)
  "Insert the given RESOURCE into the world with the given VALUE."
  (brpel-request-send-synchronously "world.insert_resources"
                                    `((resource . ,resource)
                                      (value . ,value))))

;; Method: world.remove_resources
(defun brpel-world-remove-resources (resource &optional callback)
  "Remove the given RESOURCE from the world.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.remove_resources"
                      `((resource . ,resource))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-remove-resources-synchronously (resource)
  "Remove the given RESOURCE from the world."
  (brpel-request-send-synchronously "world.remove_resources"
                      `((resource . ,resource))))

;; Method: world.mutate_resources
(defun brpel-world-mutate-resources (resource path value &optional callback)
  "Mutate a field in a RESOURCE with PATH and set it to VALUE.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.mutate_resources"
                      `((resource . ,resource)
                        (path . ,path)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-mutate-resources-synchronously (resource path value)
  "Mutate a field in a RESOURCE with PATH and set it to VALUE."
  (brpel-request-send-synchronously "world.mutate_resources"
                                    `((resource . ,resource)
                                      (path . ,path)
                                      (value . ,value))))

;; Method: world.list_resources
(defun brpel-world-list-resources (&optional callback)
  "List all reflectable registered resource types.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.list_resources" nil
                      (or callback 'brpel--default-callback)))

(defun brpel-world-list-resources-synchronously ()
  "List all reflectable registered resource types."
  (brpel-request-send-synchronously "world.list_resources" nil))

;; Method: world.trigger_event
(defun brpel-world-trigger-event (event value &optional callback)
  "Triggers an EVENT, giving the event the VALUE.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "world.trigger_event"
                      `((event . ,event)
                        (value . ,value))
                      (or callback 'brpel--default-callback)))

(defun brpel-world-trigger-event-synchronously (event value)
  "Triggers an EVENT, giving the event the VALUE."
  (brpel-request-send-synchronously "world.trigger_event"
                                    `((event . ,event)
                                      (value . ,value))))

;; Method: registry.schema
(defun brpel-registry-schema (&optional with-crates without-crates type-limit callback)
  "Retrieve schema information about registered types in the current app.
WITH-CRATES is an array of crate names to include in the result.
WITHOUT-CRATES is an array of crate names to exclude from the results.
TYPE-LIMIT contains an array of with and without
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "registry.schema"
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
  (brpel-request-send-synchronously "registry.schema"
                      (if (and with-crates without-crates type-limit)
                          `((with_crates . ,with-crates)
                            (without_crates . ,without-crates)
                            (type_limit . ,type-limit))
                        nil)))

;; NOTE This function stores typePaths that are not useful
;; in the context of this tool. They should realistically
;; be discarded. For simplicity, they are just kept in as they
;; mean no harm :)
(defun brpel--registry-schema-index-populate ()
  "Populate the `brpel--registry-schema-index'."
  (brpel-registry-schema
   nil nil nil
   (lambda (schema)
     (setq brpel--registry-schema-index (make-hash-table :test #'equal))
     (dolist (field (alist-get 'result schema))
       (let* ((type-path (alist-get 'typePath field))
              (parts (split-string type-path "::"))
              (last-part (car (last parts))))
         (puthash (string-trim last-part) type-path brpel--registry-schema-index))))))

;; Method: rpc.discover
(defun brpel-rpc-discover (&optional callback)
  "Discover available remote methods and server information.
If CALLBACK is non-nil, it will be called on the result of this command."
  (brpel-request-send "rpc.discover" nil
                      (or callback 'brpel--default-callback)))

(defun brpel-rpc-discover-synchronously ()
  "Discover available remote methods and server information."
  (brpel-request-send-synchronously "rpc.discover" nil))

(provide 'brpel-methods)
;;; brpel-methods.el ends here
