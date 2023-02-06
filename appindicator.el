;;; appindicator.el --- Create and control appindicators -*- lexical-binding:t -*-

;; Copyright (c) 2022 by Dmitriy Pshonko.

;; Author: Dmitriy Pshonko <jumper047@gmail.com>
;; URL: https://github.com/jumper047/emacs-appindicator
;; Keywords: mouse convenience
;; Version: 0.1.0

;; Package-Requires: ((emacs "27.1") (svg-lib "0.2.5"))

;; This file is NOT part of GNU Emacs

;;; License:

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

;; Library to create and control tray icons from Elisp
;; To create new tray icon run macro `appindicator-create'
;; It will create set of functions needed to interact with helper to set up
;; icon, label, visibility and context menu

;;; Code:
(require 'subr-x)
(require 'appindicator-svg)

(defvar appindicator-helper-executable-name "emacs-appindicator-helper"
  "Appindicator helper's executable name.")
(defvar appindicator--helper-min-version "0.1.0"
  "Minimal version of the appindicator helper needed by this lib.")
(defvar appindicator-compile-buffer-name "*Compile appindicator helper*"
  "Buffer name used for appindicator helper binary compilation.")

(defconst appindicator--helper-src-subdir "appindicator-helper/"
  "Sub directory inside package containing appindicator helper sources.")

(defun appindicator--helper-dir ()
  "Get directory containing appindicator helper sources and binary."
  (concat (file-name-directory (locate-library "appindicator.el"))
          appindicator--helper-src-subdir))

(defun appindicator--helper-executable ()
  "Get command to start appindicator helper."
  (let ((exec-path
         (cons (appindicator--helper-dir)
               exec-path)))
    (or (executable-find appindicator-helper-executable-name)
        (error "Can't find executable %s" appindicator-helper-executable-name))))

(defun appindicator--helper-version ()
  "Get current appindicator helper version.
Returns `0.0.0-unknown' if no binary available."
  "Return appindicator helper version."
  (let ((version-str (shell-command-to-string
                      (concat (appindicator--helper-executable) " -v"))))
    (if (string-match "^Version \\([0-9.]+\\)" version-str)
        (match-string 1 version-str)
      "0.0.0-unknown")))

(defun appindicator--build-helper ()
  "Build appindicator helper executable from sources."
  (interactive)
  (let* ((make-commands
          (concat
           "cd " (appindicator--helper-dir) ";\n make " appindicator-helper-executable-name))
         (buffer (get-buffer-create appindicator-compile-buffer-name)))
    (pop-to-buffer buffer)
    (compilation-mode)
    (if (zerop (let ((inhibit-read-only t))
                 (call-process "sh" nil buffer nil "-c" make-commands)))
        (message "Compilation of the `appindicator-helper' succeeded")
      (error "Compilation of the `appindicator-helper' failed!"))))

(defun appindicator-ensure-helper ()
  "Check if appindicator helper available and asking to build it if not."
  (let ((exec-path (cons (appindicator--helper-dir) exec-path)))
    (cond ((and (executable-find appindicator-helper-executable-name)
                (version< appindicator--helper-min-version (appindicator--helper-version)))
           (when (y-or-n-p (format "Helper version is outdated (%s<%s), rebuild?"
                                   appindicator--helper-min-version
                                   (appindicator--helper-version)))
             (appindicator--build-helper)))
          ((not (executable-find appindicator-helper-executable-name))
           (when (y-or-n-p (format "Appindicator-helper executable not found, build it?"))
             (appindicator--build-helper)))
          (t))))

;; Checking if appindicator-helper is available
(unless (appindicator-ensure-helper)
  (error "Appindicator helper is not available!"))

(defun appindicator--filter-menu (menu)
  "Get rid of separator placeholders in MENU.
Transforms menu into correct alist."
  (let (filtered)
    (dolist (menu-elt menu filtered)
      (if (listp menu-elt)
          (push menu-elt filtered)))))

(defun appindicator--menu-to-arg (menu)
  "Transform MENU list to arguments for appindicator helper."
  (let (result)
    (dolist (menu-elt (reverse menu))
      (if (listp menu-elt)
          (push (car menu-elt) result)
        (push "-" result)))
    (string-join result "|")))

(defun appindicator--send (parcel proc-buffer)
  "Send PARCEL to appindicator helper process.
PROC-BUFFER should be process buffer used by helper."
  (let* ((print-circle nil)
         (print-level nil)
         (print-length nil)
         (value (prin1-to-string (substring-no-properties parcel)))
         (proc (get-buffer-process proc-buffer)))
    (unless (process-live-p proc)
      (user-error (format "%s have no running process!" (buffer-name proc-buffer))))
    (process-send-string
     proc
     (concat (number-to-string (string-bytes value)) "\n"))
    (process-send-string proc value)
    (process-send-string proc "\n")))

(defsubst appindicator--parse-cmd ()
  "Parse command sent by appindicator-helper inside stdout buffer."
  (when (re-search-forward "^\\([0-9]+\\)\n" nil t)
    (let ((sexpsz (string-to-number (match-string 1))))
      ;; New command always start at the beginning, no garbage between commands
      (unless (= (match-beginning 0) 1)
        ;; Kill the garbage at the beginning
        (message "emacs-appindicator: garbage in process buffer!")
        (delete-region (point-min) (match-beginning 0)))

      (when (> (- (point-max) (point)) sexpsz)
        (let ((value (read (current-buffer))))
          (prog1
              value
            (delete-region (point-min) (point))

            ;; remove trailing newline
            (if (= (following-char) ?\n)
                (delete-char 1)
              (user-error "Unexpected symbol!"))))))))

(defun appindicator--parse-commands (menu-alist)
  "Parse command received from appindicator helper.
MENU-ALIST should be list with cells containing
context menu entries and callbacks for them,
like (\"Start\" . start-fn)."
  (goto-char (point-min))
  (let (cmd-val parsed-commands handler)
    (while (setq cmd-val (appindicator--parse-cmd))
      (setq parsed-commands
            (cons cmd-val
                  parsed-commands)))
    (dolist (cmd (nreverse parsed-commands))
      (setq handler (alist-get cmd menu-alist nil nil 'equal))
      (funcall handler))))

(defmacro appindicator-create (name &optional skip-init)
  "Generate functions for new appindicator helper.
This macro creates multiple functions to interact with
appindicator instance with NAME.
If optional SKIP-INIT is nil, new appindicator process
will be started immediately.
Most notable functions are:
- appindicator-NAME-init
- appindicator-NAME-kill
- appindicator-NAME-set-icon
- appindicator-NAME-set-label
- appindicator-NAME-set-active
- appindicator-NAME-set-menu"

  (let* ((prefix-fn (concat "appindicator-" name))
         (buffer-var (intern (concat prefix-fn "--buffer")))
         (menu-var (intern (concat prefix-fn "--menu")))
         (sentinel-fn (intern (concat prefix-fn "--sentinel")))
         (filter-fn (intern (concat prefix-fn name "--filter")))
         (init-fn (intern (concat prefix-fn "-init")))
         (kill-fn (intern (concat prefix-fn "-kill")))
         (set-icon-fn (intern (concat prefix-fn "-set-icon")))
         (set-label-fn (intern (concat prefix-fn "-set-label")))
         (set-active-fn (intern (concat prefix-fn "-set-active")))
         (set-menu-fn (intern (concat prefix-fn "-set-menu")))
         (proc-buffer-name (format " *%s*" name)))
    `(progn
       (defvar ,buffer-var nil)
       (defvar ,menu-var nil)

       (defun ,sentinel-fn (proc event)
         (let ((status (substring event 0 -1)) ; strip trailing \n
               (err (if (buffer-live-p (process-buffer proc))
                        (with-current-buffer (process-buffer proc) (buffer-string))
                      "")))
           ;; Notify in echo area if appindicator-helper exited abnormally
           (unless (zerop (process-exit-status proc))
             (message "[%d]appindicator-%s: %s" (process-exit-status proc) ,name status))))

       (defun ,filter-fn (proc output)
         (let ((buffer (process-buffer proc))
               (menu (appindicator--filter-menu ,menu-var)))
           (if (buffer-live-p buffer)
               (with-current-buffer buffer
                 (goto-char (point-max))
                 (insert output)
                 (appindicator--parse-commands menu)))))

       (defun ,init-fn ()
         "Start appindicator-helper process, and send init command to it."
         (when (process-live-p (get-buffer-process ,buffer-var))
           (user-error "Error: appindicator-%s already running" ,name))

         (let* ((process-connection-type nil)
                (process-adaptive-read-buffering nil)
                (server-cmd (appindicator--helper-executable)))
           (with-current-buffer (generate-new-buffer ,proc-buffer-name)
             (setq ,buffer-var (current-buffer))
             (let* ((proc-cmd-with-args (split-string server-cmd " " t))
                    (proc (apply 'start-process
                                 (format "appindicator-%s" ,name) (current-buffer)
                                 proc-cmd-with-args)))
               (set-process-query-on-exit-flag proc nil)
               (set-process-sentinel proc #',sentinel-fn)
               (set-process-filter proc #',filter-fn)
               (set-process-coding-system proc 'utf-8 'utf-8))))
         ;; Initializing appindicator
         (appindicator--send (format "init %s" ,name) ,buffer-var))

       (defun ,kill-fn ()
         "Kill process for appindicator helper"
         (interactive)
         (when (buffer-live-p ,buffer-var)
           (kill-buffer ,buffer-var)))

       (defun ,set-icon-fn (icon &optional collection &rest args)
         "Set tray icon for appindicator helper.
ICON may be path to svg file or name of the icon from COLLECTION
\(see `svg-lib' README for more details regarding collections\).
ARGS may be the following keywords:
- `:background' and `:foreground' - recolor svg icon"

         (cond ((and (not collection)
                     (not (equal "svg" (file-name-extension icon))))
                (when args (display-warning 'appindicator-svg
                                            (format "%s icon can't be customized"
                                                    (file-name-extension icon))))
                (setq icon (expand-file-name icon)))
               ;; If we don't modify svg icon, there is no need to cache it
               ((and (not collection)
                     (not args)
                     (equal "svg" (file-name-extension icon)))
                (setq icon (expand-file-name icon)))
               ('t
                (setq icon (apply 'appindicator-svg-icon ,name icon collection args))))
         (appindicator--send (format "icon %s" icon) ,buffer-var))

       (defun ,set-label-fn (label-str)
         "Set label with text LABEL-STR for appindicator.
May not work if your tray implementation doesn't support labels"
         (appindicator--send (format "label %s" label-str) ,buffer-var))

       (defun ,set-active-fn (active)
         "Hide or show appindicator icon.
ACTIVE supposed to be boolean value."
         (appindicator--send (format "status %s" (if active "active" "inactive")) ,buffer-var))

       (defun ,set-menu-fn (menu)
         "Set context menu for appindicator.
MENU should be a list, each element may be a separator
(any unknown symbol, i prefer to use word separator),
or cell with entry's name and function to launch on
entry selection, like (\"Start\" . my-custom-start-function)."
         (setq ,menu-var menu)
         (appindicator--send (format "menu %s" (appindicator--menu-to-arg menu)) ,buffer-var))

       (unless ,skip-init
         (,init-fn)
         (,set-menu-fn '())
         (,set-active-fn nil)))))


(provide 'appindicator)
;;; appindicator.el ends here
