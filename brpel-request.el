;;; brpel-request.el --- request functionality for brpel -*- lexical-binding: t; -*-
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
(require 'json)
(require 'url)

(defcustom brpel-request-url "http://localhost:15702"
  "URL of the BRP running server."
  :type 'string
  :group 'brpel)

(defvar brpel-request--id 1
  "Incrementing ID for JSON-RPC requests.")

(defun brpel-request-url-set (url)
  "Set the `brpel-request-url' variable to URL.
Attempt to connect and sync the registry-schema index."
  (setq brpel-request-url url)
  (condition-case err
      (brpel--registry-schema-index-populate)
    (error (message (car (last err))))))

(defun brpel-request--default-callback (result)
  "The default callback that simply prints the RESULT of each BRP command."
  (message (json-encode result)))

(defun brpel-request-send (method &optional params callback)
  "Send a JSON-RPC request to the BRP server.

METHOD is the BRP method as a string, see:
https://docs.rs/bevy/latest/bevy/remote/index.html for more details.
PARAMS is an alist representing the JSON object to send as `params'.
CALLBACK is a function to handle the response buffer."

  (let* ((request-id (setq brpel-request--id (1+ brpel-request--id)))
         (data (json-encode
                `((jsonrpc . "2.0")
                  (id . ,request-id)
                  (method . ,method)
                  ,@(when params `((params . ,params))))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (url-retrieve brpel-request-url
                  (lambda (status)
                    (if-let ((handle-error (plist-get status :error)))
                        (error "Failed to connect to the BRP server"))
                    (when (buffer-live-p (current-buffer))
                      (goto-char url-http-end-of-headers)
                      (let ((json-str (buffer-substring (point) (point-max))))
                        (condition-case err
                            (let ((json-res (json-read-from-string json-str)))
                              (if callback
                                  (funcall callback json-res)
                                (brpel-request--default-callback json-res)))
                          (error (message "BRP error: %s" err)))))))
    nil t))

(defun brpel-request-send-synchronously (method &optional params)
  "Send a synchronous JSON-RPC request to the BRP server.

METHOD is the BRP method as a string, see:
https://docs.rs/bevy/latest/bevy/remote/index.html for more details.
PARAMS is an alist representing the JSON object to send as `params'."

  (let* ((request-id (setq brpel-request--id (1+ brpel-request--id)))
         (data (json-encode
                `((jsonrpc . "2.0")
                  (id . ,request-id)
                  (method . ,method)
                  ,@(when params `((params . ,params))))))
         (url-request-method "POST")
         (url-request-data data)
         (url-request-extra-headers '(("Content-Type" . "application/json"))))
    (with-current-buffer (url-retrieve-synchronously brpel-request-url t t 1)
      (if-let ((blen (= 0 (buffer-size))))
          (error "Failed to connect to the BRP server"))
      (when (buffer-live-p (current-buffer))
        (goto-char url-http-end-of-headers)
        (let ((json-str (buffer-substring (point) (point-max))))
          (condition-case err
              (let ((json-res (json-read-from-string json-str)))
                json-res)
            (error (message "BRP error: %s" err))))))))

(provide 'brpel-request)
;;; brpel-request.el ends here
