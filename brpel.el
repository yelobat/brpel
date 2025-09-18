;;; brpel.el --- Implements the BRP protocol for Emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Luke Holland
;;
;; Author: Luke Holland
;; Maintainer: Luke Holland
;; Created: September 07, 2025
;; Modified: September 07, 2025
;; Version: 0.0.1
;; Keywords: brp comm data docs extensions games hardware lisp local multimedia processes tools unix
;; Homepage: https://github.com/yelobat/brp
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
(require 'url-http)
(require 'json)
(require 'json-rpc)

(defgroup brpel nil
  "Interface with a BRP server."
  :group 'brpel)

(defcustom brpel-remote-url "http://localhost:3030"
  "URL of the BRP running server."
  :type 'string
  :group 'brpel)

(defvar brpel-remote-id 1
  "Incrementing ID for JSON-RPC requests.")

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
                        (error "Failed to connect to BRP server")
                      (when (buffer-live-p (current-buffer))
                        (goto-char (point-min))
                        (search-forward "\n\n" nil t)
                        (let ((json-str (buffer-substring (point) (point-max))))
                          (condition-case err
                              (let ((json-res (json-read-from-string json-str)))
                                (when callback (funcall callback json-res)))
                            (error (message "BRP error: %s" err)))))))
                  nil t)))

(defun brpel-id (id)
  "Convert an ID in the format <Index>v<Generation> to a u64."
  (let* ((lst (mapcar #'string-to-number (split-string id "v")))
         (index (nth 0 lst))
         (generation (nth 1 lst)))
    (+ index (* generation (expt 2 32)))))

;; Method: bevy/get
(defun brpel-get (id components &optional strict)
  "Retrieve the values of one or more COMPONENTS from an entity with ID."
  (brpel-send-request "bevy/get"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/query
(defun brpel-query (data filter &optional strict)
  "Perform a query over components in the ECS.
Returning all matching entities and their associated component values.
Use DATA, FILTER and STRICT to determine the entities returned.
See https://docs.rs/bevy_remote/latest/bevy_remote/ for more details."
  (brpel-send-request "bevy/query"
                      `((data . ,data)
                        (filter . ,filter)
                        (strict . ,(or strict :json-false)))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/spawn
;; BUG: https://github.com/bevyengine/bevy/issues/20952
(defun brpel-spawn (components)
  "Create a new entity with the provided COMPONENTS.
Return the resulting entity ID."
  (brpel-send-request "bevy/spawn"
                      `((components . ,components))
                      (lambda (res)
                        (message (json-encode res)))))

;; Method: bevy/destroy
(defun brpel-destroy (id)
  "Despawn the entity with the given ID."
  (brpel-send-request "bevy/destroy"
                      `((entity . ,id))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/remove
(defun brpel-remove (id components)
  "Delete one or more COMPONENTS from an entity with ID."
  (brpel-send-request "bevy/remove"
                      `((entity . ,id)
                        (components . ,components))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/insert
;; BUG: https://github.com/bevyengine/bevy/issues/20952
(defun brpel-insert (id components)
  "Insert one or more COMPONENTS into an entity with ID."
  (brpel-send-request "bevy/insert"
                      `((entity . ,id)
                        (components . ,components))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/mutate_component
(defun brpel-mutate-component (id component path value)
  "Mutate a field in a COMPONENT for an entity with ID.
PATH represents the path of the field in the component.
VALUE represents the value to insert at PATH."
  (brpel-send-request "bevy/mutate_component"
                      `((entity . ,id)
                        (component . ,component)
                        (path . ,path)
                        (value . ,value))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/reparent
(defun brpel-reparent (ids &optional parent)
  "Assign a new PARENT to one or more entities for each ID in IDS."
  (brpel-send-request "bevy/reparent"
                      `((entities . ,ids)
                        (parent . ,parent))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/list
(defun brpel-list (id)
  "List all registered components or all components present on an entity with ID."
  (brpel-send-request "bevy/list"
                      (if id `((entity . ,id)))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/get+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-get+watch (id components &optional strict)
  "Watch the values of one or more COMPONENTS from an entity with ID."
  (brpel-send-request "bevy/get+watch"
                      `((entity . ,id)
                        (components . ,components)
                        (strict . ,(or strict :json-false)))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/list+watch
;; TODO Need to figure out how to best make use of this function.
(defun brpel-list+watch (id)
  "Watch all components present on an entity with ID."
  (brpel-send-request "bevy/list+watch"
                      `((entity . ,id))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/get_resource
(defun brpel-get-resource (resource-name)
  "Extract the value of a given resource with RESOURCE-NAME from the world."
  (interactive "sResource name: ")
  (brpel-send-request "bevy/get_resource"
                    `((resource . ,resource-name))
                    (lambda (res)
                      (message (json-encode res)))))

;; Method: bevy/insert_resource
(defun brpel-insert-resource (resource value)
  "Extract the VALUE of a given RESOURCE from the world."
  (brpel-send-request "bevy/insert_resource"
                      `((resource . ,resource)
                        (value . ,value))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/remove_resource
(defun brpel-remove-resource (resource)
  "Remove the given RESOURCE from the world."
  (brpel-send-request "bevy/remove_resource"
                      `((resource . ,resource))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/mutate_resource
(defun brpel-mutate-resource (resource path value)
  "Mutate a field in a RESOURCE.
PATH is the path to the field within the RESOURCE.
VALUE is the value to be inserted at PATH."
  (brpel-send-request "bevy/mutate_resource"
                      `((resource . ,resource)
                        (path . ,path)
                        (value . ,value))
                      (lambda (res) (message (json-encode res)))))

;; Method: bevy/list_resources
(defun brpel-list-resources ()
  "List all reflectable registered resource types on the BRP server."
  (interactive)
  (brpel-send-request "bevy/list_resources" nil
                      (lambda (res) (message "%s" (json-encode res)))))

(provide 'brpel)
;;; brpel.el ends here
