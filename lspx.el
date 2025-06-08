;;; lspx.el --- Organize LSP Clients  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 meowking<mr.meowking@posteo.com>

;; Version: 0.1.0
;; Author: meowking mr.meowking@posteo.com
;; Keywords: convenience
;; URL: https://codeberg.org/meow_king/lspx
;; License: GNU General Public License >= 3
;; Package-Requires: ((emacs "29.1"))

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

;; The reason why I create this project:
;; I find it hard to setup Vue Language Server(shit) in Eglot, and 
;; failed to set it correctly
;; -> use lsp-mode -> find it works out of the box !! happy!
;; -> performance issue (blocks the entire UI, even though I uses emacs-lsp-booster) :(
;; -> use lsp-bridge


;;; Code:

(require 'eglot)
(require 'lsp-mode nil t)
(require 'lsp-bridge nil t)

(defgroup lspx nil
  "Lspx."
  :prefix "lspx"
  :group 'convenience)

(defclass lspx-client ()
  ((identifier
    :initarg :id
    :type symbol :custom symbol
    :documentation "The identifier/name of a LSP client.")
   (enable-auto-startup
    :initarg :enable-auto-startup
    :initform t
    :type boolean :custom boolean
    :documentation "Whether to enable automatically start up language server
for managed major mode in a project.
For example, for Eglot, this option should be set to nil since Eglot handles
it itself.")
   (startup-fn
    :initarg :startup-fn
    :type function :custom function
    :documentation "Function for turning on client in a specific buffer.")
   (shutdown-fn
    :initarg :shutdown-fn
    :type function :custom function
    :documentation "Function for turning off client in a specific buffer.")
   ;; (:shutdown-all-fn
   ;;  :initarg :shutdown-all-fn
   ;;  :type function
   ;;  :custom function
   ;;  :documentation "Shutdown all language servers managed by this LSP client.")
   (check-alive-in-buffer-fn
    :initarg :check-alive-in-buffer-fn
    :type function :custom function
    :documentation "Function for checking whether a client is alive in a specific buffer.")
   
   
   (lsp-rename-fn
    :initarg :lsp-rename-fn
    :initform nil
    :type function :custom function)
   (lsp-find-definition-fn
    :initarg :lsp-find-definition-fn
    :initform nil
    :type function :custom function)
   (lsp-find-definition-other-window-fn
    :initarg :lsp-find-definition-other-window-fn
    :initform nil
    :type function :custom function)
   (lsp-find-type-definition-fn
    :initarg :lsp-find-type-definition-fn
    :initform nil
    :type function :custom function)
   (lsp-find-type-definition-other-window-fn
    :initarg :lsp-find-type-definition-other-window-fn
    :initform nil
    :type function :custom function)
   (lsp-find-references-fn
    :initarg :lsp-find-references-fn
    :initform nil
    :type function :custom function)
   (lsp-find-implementation-fn
    :initarg :lsp-find-implementation-fn
    :initform nil
    :type function :custom function)
   (lsp-toggle-inlay-hint-fn
    :initarg :lsp-toggle-inlay-hint-fn
    :initform nil
    :type function :custom function)
   (lsp-show-buffer-errors-fn
    :initarg :lsp-show-buffer-errors-fn
    :initform nil
    :type function :custom function)
   (lsp-execute-code-action-fn
    :initarg :lsp-execute-code-action-fn
    :initform nil
    :type function :custom function)
   (lsp-show-documentation-fn
    :initarg :lsp-show-documentation-fn
    :initform nil
    :type function :custom function)
   (lsp-format-buffer-fn
    :initarg :lsp-format-buffer-fn
    :initform nil
    :type function :custom function)
   (lsp-format-region-fn
    :initarg :lsp-format-region-fn
    :initform nil
    :type function :custom function))
  "A class for defining LSP client."
  :allow-nil-initform t)

(defun lspx--display-buffer-in-other-window ()
  "Display current buffer in the other window."
  (let ((buf (current-buffer))
        (window (selected-window)))
    (with-selected-window
        (display-buffer
         buf
         `((xref--display-buffer-in-other-window)
           (window . ,window)))
      (selected-window))))

(defun lspx--eglot-find-type-definition-other-window ()
  (interactive)
  (let (has-find target-window)
    ;; TODO wrong behavior: eglot-find-typeDefinition jumps to same buffer different position
    (setq has-find (not (string-match-p
                         "returned no"
                         (or (eglot-find-typeDefinition) ""))))
    (when has-find
      (setq target-window (lspx--display-buffer-in-other-window))
      (previous-buffer)
      (select-window target-window))))

(defun lspx--lsp-mode-find-type-definition-other-window ()
  (interactive)
  (let (has-find target-window)
    ;; TODO wrong behavior: eglot-find-typeDefinition jumps to same buffer different position
    (setq has-find (not (string-match-p
                         "not found"
                         (or (lsp-find-type-definition) ""))))
    (when has-find
      (setq target-window (lspx--display-buffer-in-other-window))
      (previous-buffer)
      (select-window target-window))))


(defconst lspx-eglot-client
  (lspx-client
   :id 'eglot
   :enable-auto-startup nil
   :startup-fn #'eglot
   :shutdown-fn #'eglot-shutdown
   :check-alive-in-buffer-fn #'eglot-managed-p
   
   :lsp-rename-fn #'eglot-rename
   :lsp-find-definition-fn #'xref-find-definitions
   :lsp-find-definition-other-window-fn #'xref-find-definitions-other-window
   :lsp-find-type-definition-fn #'eglot-find-typeDefinition
   :lsp-find-type-definition-other-window-fn #'lspx--eglot-find-type-definition-other-window
   :lsp-find-references-fn #'xref-find-references
   :lsp-find-implementation-fn #'eglot-find-implementation
   :lsp-toggle-inlay-hint-fn #'eglot-inlay-hints-mode
   :lsp-show-buffer-errors-fn #'flymake-show-buffer-diagnostics
   :lsp-execute-code-action-fn #'eglot-code-actions
   :lsp-show-documentation-fn #'eldoc
   :lsp-format-buffer-fn #'eglot-format-buffer
   :lsp-format-region-fn #'eglot-format))

(defconst lspx-lsp-mode-client
  (lspx-client
   :id 'lsp-mode
   :enable-auto-startup t
   :startup-fn #'lsp-deferred
   :shutdown-fn #'lsp-workspace-shutdown
   :check-alive-in-buffer-fn (lambda () lsp-mode)

   :lsp-rename-fn #'lsp-rename
   :lsp-find-definition-fn #'xref-find-definitions
   :lsp-find-definition-other-window-fn #'xref-find-definitions-other-window
   :lsp-find-type-definition-fn #'lsp-find-type-definition
   :lsp-find-type-definition-other-window-fn #'lspx--lsp-mode-find-type-definition-other-window
   :lsp-find-references-fn #'xref-find-references
   :lsp-find-implementation-fn #'lsp-find-implementation
   :lsp-toggle-inlay-hint-fn #'lsp-inlay-hints-mode
   :lsp-show-buffer-errors-fn #'flymake-show-buffer-diagnostics
   :lsp-execute-code-action-fn #'lsp-execute-code-action
   :lsp-show-documentation-fn #'eldoc
   :lsp-format-buffer-fn #'lsp-format-buffer
   :lsp-format-region-fn #'lsp-format-region))

(defconst lspx-lsp-bridge-client
  (lspx-client
   :id 'lsp-bridge
   :enable-auto-startup t
   :startup-fn #'lsp-bridge-mode
   ;; TODO only kill one language server (for one major mode in a project)
   ;; instead of the whole lsp-bridge process
   :shutdown-fn #'lsp-bridge-kill-process
   :check-alive-in-buffer-fn (lambda () lsp-bridge-mode)

   :lsp-rename-fn #'lsp-bridge-rename
   :lsp-find-definition-fn #'lsp-bridge-find-def
   :lsp-find-definition-other-window-fn #'lsp-bridge-find-def-other-window
   :lsp-find-type-definition-fn #'lsp-bridge-find-type-def
   :lsp-find-type-definition-other-window-fn #'lsp-bridge-find-type-def-other-window
   :lsp-find-references-fn #'lsp-bridge-find-references
   :lsp-find-implementation-fn #'lsp-bridge-find-impl
   ;; :lsp-toggle-inlay-hint-fn nil
   :lsp-show-buffer-errors-fn #'lsp-bridge-diagnostic-list
   :lsp-execute-code-action-fn #'lsp-bridge-code-action
   :lsp-show-documentation-fn #'lsp-bridge-popup-documentation
   ;; :lsp-format-region-fn nil
   :lsp-format-buffer-fn #'lsp-bridge-code-format))

(defcustom lspx-clients
  (list lspx-eglot-client lspx-lsp-mode-client lspx-lsp-bridge-client)
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

(defun lspx--cur-client ()
  "Get current LSP client for current major-mode in current project.
NOTE that `project-current' can produce error while being evaluated.  To avoid
this run-time error, use `lspx--maybe-cur-client' instead."
  (gethash (lspx--alive-client-map-key) lspx--alive-client-map))

(defun lspx--maybe-alive-client-map-key ()
  (when-let* ((project (project-current))
              (project-root-path (project-root project)))
    (cons project-root-path major-mode)))

(defun lspx--maybe-cur-client ()
  (when-let ((key (lspx--maybe-alive-client-map-key)))
    (gethash key lspx--alive-client-map)))


(defun lspx--cur-client-user-error ()
  "A variant of `lsp--cur-client' that produce user-error when no client."
  (let ((client (lspx--cur-client)))
    (unless client
      (user-error "No LSP client is handling this major mode in the project"))
    client))

(defun lspx--funcall-maybe-interactively (fn)
  "Call function FN interactively if its a command, or normally if not."
  (if (commandp fn)
      (call-interactively fn)
    (funcall fn)))

;;;###autoload
(defun lspx (client)
  "Start LSP client for current mode for current project.
CLIENT."
  (interactive
   (progn
     (when-let ((cur-client (lspx--cur-client)))
       (user-error "LSP client %s is currently handling this major mode in the project!"
                   (slot-value cur-client 'identifier)))
     (list (lspx--choose-client))))

  (unless (called-interactively-p 'interactive)
    (when-let ((cur-client (lspx--cur-client)))
      (user-error "LSP client %s is currently handling this major mode in the project!"
                  (slot-value cur-client 'identifier))))
  
  (lspx--funcall-maybe-interactively (slot-value client 'startup-fn))
  (puthash (lspx--alive-client-map-key) client lspx--alive-client-map))

;;;###autoload
(defun lspx-shutdown ()
  (interactive)
  (let ((client (lspx--cur-client-user-error)))
    (unwind-protect
        (lspx--funcall-maybe-interactively (slot-value client 'shutdown-fn))
      (remhash (lspx--alive-client-map-key) lspx--alive-client-map))))

(defun lspx--maybe-start-lsp ()
  (when-let ((client (lspx--maybe-cur-client)))
    (when (and (slot-value client 'enable-auto-startup)
               (not (funcall (slot-value client 'check-alive-in-buffer-fn))))
      (lspx--funcall-maybe-interactively (slot-value client 'startup-fn)))))


(defun lspx-setup-lspx()
  (add-hook 'after-change-major-mode-hook #'lspx--maybe-start-lsp))


(defun lspx--execute-lsp-fn (attr)
  (let* ((client (lspx--cur-client-user-error))
         (fn (slot-value client attr))
         (xref-prompt-for-identifier nil))
    (unless fn
      (user-error
       "LSP client %s doesn't support feature %s or is not configured properly"
       (slot-value client 'identifier) attr))
    (lspx--funcall-maybe-interactively fn)))

;;;###autoload
(defun lspx-rename ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-rename-fn))


;;;###autoload
(defun lspx-find-definition ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-definition-fn))

;;;###autoload
(defun lspx-find-definition-xref ()
  "Use `xref-find-definitions' as backup function for `lspx-find-definition'."
  (interactive)
  (let ((xref-prompt-for-identifier nil))
    (condition-case nil
        (lspx-find-definition)
      (error (call-interactively #'xref-find-definitions)))))

;;;###autoload
(defun lspx-find-definition-other-window ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-definition-other-window-fn))

;;;###autoload
(defun lspx-find-definition-other-window-xref ()
  "Use `xref-find-definitions-other-window' as backup function."
  (interactive)
  (let ((xref-prompt-for-identifier nil))
    (condition-case nil
        (lspx-find-definition-other-window)
      (error (call-interactively #'xref-find-definitions-other-window)))))


;;;###autoload
(defun lspx-find-type-definition ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-type-definition-fn))


;;;###autoload
(defun lspx-find-type-definition-other-window ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-type-definition-other-window-fn))

;;;###autoload
(defun lspx-find-references ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-references-fn))

;;;###autoload
(defun lspx-find-references-xref ()
  "Use `xref-find-references' as backup function for `lspx-find-references'."
  (interactive)
  (let ((xref-prompt-for-identifier nil))
    (condition-case nil
        (lspx-find-references)
      (error (call-interactively #'xref-find-references)))))


;;;###autoload
(defun lspx-find-implementation ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-find-implementation-fn))

;;;###autoload
(defun lspx-toggle-inlay-hint ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-toggle-inlay-hint-fn))


;;;###autoload
(defun lspx-show-buffer-errors ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-show-buffer-errors-fn))



;;;###autoload
(defun lspx-execute-code-action ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-execute-code-action-fn))


;;;###autoload
(defun lspx-show-documentation ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-show-documentation-fn))


;;;###autoload
(defun lspx-format-buffer ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-format-buffer-fn))

;;;###autoload
(defun lspx-format-region ()
  (interactive)
  (lspx--execute-lsp-fn 'lsp-format-region-fn))





(provide 'lspx)

;;; lspx.el ends here
