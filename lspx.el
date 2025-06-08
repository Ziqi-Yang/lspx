;;; lspx.el --- Organize Emacs LSP Clients  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 meowking<mr.meowking@posteo.com>

;; Version: 0.1.0
;; Author: meowking mr.meowking@posteo.com
;; Keywords: convenience
;; URL: https://codeberg.org/meow_king/lspx
;; License: GNU General Public License >= 3
;; Package-Requires: ()  ;FIXME: `package-lint-current-buffer'

;; This file is NOT part of Emacs.
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Organize Emacs LSP Clients

;; Currently only support project.el integration

;;; Code:

(require 'eglot)
(require 'lsp-mode nil t)

(defgroup lspx nil
  "Lspx."
  :prefix "lspx")

(defclass lspx-client ()
  ((identifier
    :initarg :id
    :type symbol
    :custom symbol
    :documentation "The identifier/name of a LSP client.")
   (startup-fn
    :initarg :startup-fn
    :type function
    :custom function
    :documentation "Function for turning on client in a specific buffer.")
   (shutdown-fn
    :initarg :shutdown-fn
    :type function
    :custom function
    :documentation "Function for turning off client in a specific buffer.")
   ;; (:shutdown-all-fn
   ;;  )
   (check-alive-fn
    :initarg :check-alive-fn
    :type function
    :custom function
    :documentation "Function for checking whether a client is alive in a specific buffer."))
  "A class for defining LSP client.")

(defconst lspx-eglot-client
  (lspx-client
   :id 'eglot
   :startup-fn #'eglot
   :shutdown-fn #'eglot-shutdown
   :check-alive-fn (lambda () ())))

(defconst lspx-lsp-mode-client
  (lspx-client
   :id 'lsp-mode
   :startup-fn #'lsp-deferred
   :shutdown-fn #'lsp-workspace-shutdown
   :check-alive-fn (lambda () lsp-mode)))

(defcustom lspx-clients
  (list lspx-eglot-client lspx-lsp-mode-client)
  "A list of supported LSP clients."
  :type (list lspx-client)
  :group 'lspx)

(defvar lspx--alive-client-map (make-hash-table :test #'equal)
  "Hash table: project_root_path + major mode -> client identifier.")

(defun lspx--choose-client ()
  "Choose a LSP client."
  (interactive)
  (let ((choices (mapcar (lambda (client) (cons (slot-value client 'identifier) client))
                         lspx-clients)))
    (alist-get
     (intern
      (completing-read
       (format "Choose an LSP Client: ")
       choices))
     choices)))

(defun lspx--alive-client-map-key ()
  (cons (project-root (project-current)) major-mode))

(defun lspx--cur-alive-client ()
  (gethash (lspx--alive-client-map-key) lspx--alive-client-map))

(defun lspx--funcall (fn)
  "Call function FN interactively if its a command, or normally if not."
  (if (commandp fn)
      (call-interactively fn)
    (funcall fn)))

(defun lspx (client)
  "Start LSP client for current mode for current project.
CLIENT."
  (interactive
   (progn
     (when-let ((cur-client (lspx--cur-alive-client)))
       (user-error "LSP client %s is currently handling this major mode in the project!"
                   (slot-value cur-client 'identifier)))
     (list (lspx--choose-client))))

  (unless (called-interactively-p 'interactive)
    (when-let ((cur-client (lspx--cur-alive-client)))
      (user-error "LSP client %s is currently handling this major mode in the project!"
                  (slot-value cur-client 'identifier))))
  
  (lspx--funcall (slot-value client 'startup-fn))
  (puthash (lspx--alive-client-map-key) client lspx--alive-client-map))

(defun lspx-shutdown ()
  (interactive)
  (let ((client (lspx--cur-alive-client)))
    (if (not client)
        (user-error "No LSP client is handling this major mode in the project")
      (unwind-protect
          (lspx--funcall (slot-value client 'shutdown-fn))
        (remhash (lspx--alive-client-map-key) lspx--alive-client-map)))))

(defun lspx-shutdown-all ()
  (interactive)
  ())



(provide 'lspx)

;;; lspx.el ends here
