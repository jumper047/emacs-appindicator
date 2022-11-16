;;; ellit-org: minor-modes
;; ** telega-appindicator-mode
;;
;; Global minor mode to display =telega= status in system tray.  This
;; mode requires appindicator support in the =telega-server=.  To add
;; appindicator support to =telega-server=, please install
;; =libappindicator3-dev= system package and rebuild =telega-server=
;; with {{{kbd(M-x telega-server-build RET}}}.
;;
;; Screenshot of system tray with enabled =telega= appindicator:
;; [[https://zevlg.github.io/telega/screen-appindicator.png]]
;;
;; Enable with ~(telega-appindicator-mode 1)~ or at =telega= load time:
;; #+begin_src emacs-lisp
;; (add-hook 'telega-load-hook 'telega-appindicator-mode)
;; #+end_src
;;
;; Customizable options:
;; - {{{user-option(telega-appindicator-use-label, 2)}}}
;; - {{{user-option(telega-appindicator-icon-colors, 2)}}}
;; - {{{user-option(telega-appindicator-show-account-name, 2)}}}
;; - {{{user-option(telega-appindicator-show-mentions, 2)}}}
;; - {{{user-option(telega-appindicator-labels, 2)}}}
(require 'cl-lib)

;; (defcustom slack-notifier-use-label nil
;;   "Non-nil to add text labels to the icon.
;; Otherwise use just icon to show info.
;; labels are not supported by XEMBED based system trays, such as
;; `exwm-systemtray' or `polybar'."
;;   :type 'boolean
;;   :group 'slack-notifier)

(defvar slack-notifier-new-messages-icon)

(defvar slack-notifier-no-messages-icon)




;;;###autoload
(define-minor-mode slack-notifier-mode
  "Toggle display of the unread chats/mentions in the system tray."
  :init-value nil :global t :group 'slack-notifier

  (if slack-notifier-mode
      (progn
        ;; (advice-add 'telega--on-updateUnreadChatCount
        ;;             :after 'telega-appindicator-update)
        ;; (advice-add 'telega--on-updateChatUnreadMentionCount
        ;;             :after 'telega-appindicator-update)
        ;; (add-hook 'telega-ready-hook 'telega-appindicator-init)
        ;; (add-hook 'telega-chats-fetched-hook 'telega-appindicator-update)
        ;; (add-hook 'telega-online-status-hook 'telega-appindicator-update)
        ;; (add-hook 'telega-connection-state-hook 'telega-appindicator-update)
        (when (slack-notifier-live-p)
          (slack-notifier-init)))

    ;; (remove-hook 'telega-connection-state-hook 'telega-appindicator-update)
    ;; (remove-hook 'telega-online-status-hook 'telega-appindicator-update)
    ;; (remove-hook 'telega-chats-fetched-hook 'telega-appindicator-update)
    ;; (remove-hook 'telega-ready-hook 'telega-appindicator-init)
    ;; (advice-remove 'telega--on-updateChatUnreadMentionCount
    ;;                'telega-appindicator-update)
    ;; (advice-remove 'telega--on-updateUnreadChatCount
    ;;                'telega-appindicator-update)
    ;; Deactivate appindicator
    (when (slack-notifier-live-p)
      (slack-notifier--send "status passive" "appindicator"))
    ))



(defun slack-notifier-init ()
  "Initialize appindicator."
  (when slack-notifier-mode
    (slack-notifier--send
     (concat "setup " (slack-notifier--gen-svg-icon))
     "appindicator")
    (slack-notifier-update)))

(defun slack-notifier-update (&rest _ignored)
  "Update appindicator label."
  (when slack-notifier-mode
    (let* ((account
            (when (and telega-appindicator-use-label
                       telega-appindicator-show-account-name)
              (car (telega-account-current))))
           (uu-chats-num
            (or (plist-get telega--unread-chat-count :unread_unmuted_count)
                0))
           (uu-chats-str
            (unless (zerop uu-chats-num)
              (or (nth (1- uu-chats-num) telega-appindicator-labels)
                  (number-to-string uu-chats-num))))
           (mentions-num
            (or (when (and telega-appindicator-use-label
                           telega-appindicator-show-mentions)
                  (length (telega-filter-chats
                           telega--ordered-chats '(mention))))
                0))
           (mentions-str
            (unless (zerop mentions-num)
              (format "@%d" mentions-num)))
           (label-strings
            (remove nil (list account
                              (when (and account
                                         (or uu-chats-str mentions-str))
                                "-")
                              uu-chats-str
                              mentions-str)))
           (new-label (mapconcat #'identity label-strings " "))
           (icon-filename
            (telega-appindicator--gen-svg-icon
             (unless telega-appindicator-use-label
               new-label))))
      (telega-server--send (concat "icon " icon-filename) "appindicator")
      (when telega-appindicator-use-label
        (telega-server--send (concat "label " new-label) "appindicator")))))

(defun slack-notifier--on-event (event)
  "Function called when event from appindicator is received."
  (cond ((string= event "open")
         ;; NOTE: Raise Emacs frame and open rootbuf
         (x-focus-frame nil)
         (telega)
         ;; If there is single important chat, then switch to it
         (let ((ichats (telega-filter-chats telega--ordered-chats 'important)))
           (when (= 1 (length ichats))
             (telega-switch-important-chat (car ichats)))))

        ((string= event "quit")
         (x-focus-frame nil)
         (telega-kill nil))

        (t
         (message "telega-server: Unknown appindicator-event: %s" event))))


;; Server runtime vars
(defvar slack-notifier--buffer nil)
(defvar telega-server--extra 0 "Value for :@extra used by `telega-server--call'.")
(defvar telega-server--callbacks nil "Callbacks ruled by extra")
(defvar telega-server--results nil)
(defvar telega-server--on-event-func #'telega--on-event
  "Func used to trigger on event.
Used to make deferred calls.")
(defvar telega-server--deferred-events nil)
(defvar telega-server--inhibit-events nil
  "List of events to ignore.
Bind this to avoid processing some events, while executing something.")

(defvar telega-server--idle-timer nil
  "Timer to run `telega-handle-emacs-idle' after some data is received.")
(defvar telega-server-idle-delay 0.1
  "Idle delay to process dirtiness.")

(defun telega--on-deferred-event (event)
  (setq telega-server--deferred-events
        (nconc telega-server--deferred-events (list event))))

(defmacro with-telega-deferred-events (&rest body)
  "Execute BODY deferring telega-server events processing.
Events processing can be deferred only once.
If already deferring, then just executes the BODY."
  (declare (indent 0))
  (let ((evsym (gensym "event")))
    `(if (eq telega-server--on-event-func 'telega--on-deferred-event)
         (progn ,@body)

       (setq telega-server--on-event-func 'telega--on-deferred-event)
       (unwind-protect
           (progn ,@body)

         (unwind-protect
             (while telega-server--deferred-events
               (let ((,evsym (car telega-server--deferred-events)))
                 (telega-debug "%s event: %S"
                               (propertize "DEFERRED" 'face 'bold) ,evsym)
                 (setq telega-server--deferred-events
                       (cdr telega-server--deferred-events))
                 (telega--on-event ,evsym)))

           (setq telega-server--deferred-events nil
                 telega-server--on-event-func 'telega--on-event)
           )))))

(defmacro telega-server--callback-put (extra cb)
  `(puthash ,extra ,cb telega-server--callbacks))

(defmacro telega-server--callback-rm (extra)
  `(remhash ,extra telega-server--callbacks))

(defmacro telega-server--callback-get (extra)
  `(gethash ,extra telega-server--callbacks))

(defun slack-notifier-test-env (&optional quiet-p)
  "Test Emacs environment.
If QUIET-P is non-nil, then show success message in echo area.
Return non-nil if all tests are passed."
  (interactive "P")
  ;; 62bits for numbers is required
  ;; i.e. ./configure --with-wide-int
  (cl-assert (= most-positive-fixnum 2305843009213693951) nil
             "Emacs with wide ints (--with-wide-int) is required")
  (cl-assert (= (string-to-number "542353335") 542353335) nil
             (concat "Emacs with `(string-to-number \"542353335\") ==> 542353335'"
                     " is required"))

  ;; at least 25.1 emacs is required
  ;; see https://t.me/emacs_telega/1592
  (cl-assert (fboundp 'cursor-intangible-mode) nil
             "Emacs with `cursor-intangible-mode' is required")

  ;; For now stick with at least 26.1 Emacs
  (cl-assert (string-version-lessp "26.0" emacs-version) nil
             (format "At least Emacs 26.0 is required, but you have %s"
                     emacs-version))

  ;; imagemagick for images NOT required, we have now fallback in case
  ;; native image transforms available (newer Emacs)
  (cl-assert (image-type-available-p 'imagemagick)
             nil
             (concat "Emacs with `imagemagick' support is required."
                     " (libmagickcore, libmagickwand, --with-imagemagick)"))
  
  (unless quiet-p
    (message "Your Emacs is suitable for slack notifier"))
  t)

(setq slack-notifier--lib-directory
      (or (and load-file-name
               (file-name-directory load-file-name))
          default-directory))

;;;###autoload
(defun slack-notifier-build (&optional build-flags)
  "Build and install `slack-notifier' binary.
If BUILD-FLAGS is specified, then rebuild server without any
queries using this flags for building, could be empty string.
Otherwise query user about building flags."
  (interactive)
  (slack-notifier-test-env 'quiet)
  (when (or build-flags
            (y-or-n-p "Build `slack-notifier'? "))
    ;; NOTE: check header files exists before running compilation
    (message "Slack notifier: building binary...")
    (let ((default-directory slack-notifier--lib-directory))
      (unless build-flags
        (setq build-flags
              (concat
               ;; NOTE: Do not ask about VOIP support, because there
               ;; is no support for it yet
               (when (and nil
                          (y-or-n-p "Build `telega-server' with VOIP support? "))
                 " WITH_VOIP=t")
               ;; NOTE: TON is postponed, see https://t.me/durov/116
               ;; So do not ask for TON support
               )))
      (unless (zerop
               (shell-command
                (concat (or (executable-find "gmake")
                            "make")
                        " " build-flags " "
                        "LIBS_PREFIX=" (expand-file-name telega-server-libs-prefix) " "
                        "INSTALL_PREFIX=" (expand-file-name telega-directory) " "
                        "server-reinstall")))
        (error "`telega-server' installation failed"))
      (message "Telega: building telega-server...DONE"))))

(defun slack-notifier--ensure-build ()
  "Make sure slack-notifier is build and can run."
  (let ((exec-path (cons slack-notifier-directory exec-path)))
    (or (if (executable-find slack-notifier-command)
            (slack-notifier--check-version)
          (slack-notifier-build))
        (executable-find slack-notifier-command)
        (error "`%s' not found in exec-path" slack-notifier-command))))

(defun slack-notifier--process-command (&rest flags)
  "Create command to start `slack-notifier' progress.
FLAGS - additional.
Raise error if not found."
  (let ((exec-path (cons slack-notifier-directory exec-path)))
    (or (executable-find slack-notifier-command)
        (error "`%s' not found in exec-path"
               slack-notifier-command))))

(defun slack-notifier-version ()
  "Return slack-notifier version."
  (let ((sn-usage (shell-command-to-string
                   (slack-notifier--process-command "-h"))))
    (when (string-match "^Version \\([0-9.]+\\)" sn-usage)
      (match-string 1 sn-usage))))

(defvar slack-notifier-min-version)
(defun slack-notifier--check-version ()
  "Check telega-server version against `slack-notifier-min-version'.
If does not match, then query user to rebuild slack-notifier."
  (let ((sn-version (or (telega-server-version) "0.0.0-unknown")))
    (when (and (version< sn-version slack-notifier-min-version)
               (y-or-n-p
                (format "Installed `slack-notifier' version %s<%s, rebuild? "
                        sn-version slack-notifier-min-version)))
      ;; NOTE: remove old slack-notifier binary before rebuilding
      (let* ((sv-ver (car (split-string
                           (shell-command-to-string
                            (telega-server--process-command "-h"))
                           "\n"))))
        (slack-notifier-build)))))

(defsubst slack-notifier--proc ()
  "Return telega-server process."
  (get-buffer-process slack-notifier--buffer))

(defun slack-notifier-live-p ()
  "Return non-nil if telega-sever process is alive."
  (process-live-p (slack-notifier--proc)))

(defsubst slack-notifier--tl-unpack (obj)
  "Unpack TL object OBJ."
  obj)

(defsubst slack-notifier--parse-cmd ()
  "Parse single reply from telega-server.
Return parsed command."
  (when (re-search-forward "^\\([a-z-]+\\) \\([0-9]+\\)\n" nil t)
    (let ((cmd (match-string 1))
          (sexpsz (string-to-number (match-string 2))))
      ;; New command always start at the beginning, no garbage inbetween
      ;; commands
      (unless (= (match-beginning 0) 1)
        ;; Kill the garbage at the beginning
        (message "Slack notifier: !GARBAGE! in the slack-notifier buffer")
        (delete-region (point-min) (match-beginning 0)))

      (when (> (- (point-max) (point)) sexpsz)
        (let ((value (read (current-buffer))))
          (prog1
              (list cmd (slack-notifier--tl-unpack value))
            (delete-region (point-min) (point))

            ;; remove trailing newline
            (cl-assert (= (following-char) ?\n))
            (delete-char 1)))))))

(defvar slack-notifier--last-error)
(defsubst slack-notifier--dispatch-cmd (cmd value)
  "Dispatch command CMD."

  (cond  ((string= cmd "appindicator-event")
         (telega-appindicator--on-event value))

        (t
         (error "Unknown cmd from telega-server: %s" cmd))))

(defun slack-notifier--parse-commands ()
  "Parse all available events from telega-server."
  (goto-char (point-min))
  (let (cmd-val parsed-commands)
    ;; NOTE: First parse all commands, then optimize events, because
    ;; some events (such as `updateFile', `updateChatLastMessage',
    ;; etc) can be collapsed to a single event (last one), then dispatch
    ;; all the events left after optimization
    (while (setq cmd-val (slack-notifier--parse-cmd))
      (setq parsed-commands
            (cons cmd-val
                  parsed-commands)))
    (dolist (cmd (nreverse parsed-commands))
      (apply #'slack-notifier--dispatch-cmd cmd))
    
    ))

(defun slack-notifier--filter (proc output)
  "Filter for the slack-notifier process."
  (let ((buffer (process-buffer proc)))
    (if (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert output)
          (slack-notifier--parse-commands))

      ;; telega-server buffer is killed, but telega-server process
      ;; still sends us some events
      ;;
      ;; NOTE: it causes problems when you quit telega in the middle
      ;; of chats updates, so commented out
      ;;
      ;; (with-temp-buffer
      ;;   (insert output)
      ;;   (telega-server--parse-commands))
      )))

(defun slack-notifier--sentinel (proc event)
  "Sentinel for the telega-server process."
  (let ((status (substring event 0 -1)) ; strip trailing \n
        (err (if (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc) (buffer-string))
               "")))
    ;; Notify in echo area if slack-notifier exited abnormally
    (unless (zerop (process-exit-status proc))
      (message "[%d]slack-notifier: %s" (process-exit-status proc) status))))

(defsubst slack-notifier--tl-pack (obj)
  "Pack object OBJ."
  ;; Remove text props from strings, etc
  (cond ((stringp obj) (substring-no-properties obj))
        ((vectorp obj) (cl-map 'vector #'slack-notifier--tl-pack obj))
        ((listp obj) (mapcar #'slack-notifier--tl-pack obj))
        (t obj)))

(defun slack-notifier--send (sexp &optional command)
  "Send SEXP to telega-server."
  (let* ((print-circle nil)
         (print-level nil)
         (print-length nil)
         (sexp-packed (slack-notifier--tl-pack sexp))
         (value (prin1-to-string sexp-packed))
         (proc (slack-notifier--proc)))
    (cl-assert (process-live-p proc) nil "slack-notifier is not running")
    (message "%s: %s %d %s"
                  (propertize "OUTPUT" 'face 'bold)
                  (or command "send") (string-bytes value)
                  value)

    (process-send-string
     proc
     (concat (or command "send") " "
             (number-to-string (string-bytes value)) "\n"))
    (process-send-string proc value)
    (process-send-string proc "\n")))

(defun slack-notifier--start ()
  "Start telega-server process."
  (when (process-live-p (slack-notifier--proc))
    (user-error "Error: slack-notifier already running"))

  ;; (cl-assert (buffer-live-p (telega-root--buffer)) nil
  ;;            "Use M-x telega RET to start telega")

  (let* ((process-connection-type nil)
         (process-adaptive-read-buffering nil)
         (server-cmd "/home/psh/Src/Linux/my/slack-appindicator/slack-notifier/slack-notifier"
	  ;; (slack-notifier--process-command)
		     ))
    (with-current-buffer (generate-new-buffer " *slack-notifier*")

      ;; (setq telega-server--on-event-func 'telega--on-event)
      ;; (setq telega-server--deferred-events nil)
      ;; (setq telega-server--inhibit-events nil)
      ;; (setq telega-server--extra 0)
      ;; (setq telega-server--callbacks (make-hash-table :test 'eq))
      ;; (setq telega-server--results (make-hash-table :test 'eq))

      (setq slack-notifier--buffer (current-buffer))

      (let* ((proc-cmd-with-args (split-string server-cmd " " t))
             (proc (apply 'start-process
                          "slack-notifier" (current-buffer)
                          proc-cmd-with-args)))
        (set-process-query-on-exit-flag proc nil)
        (set-process-sentinel proc #'slack-notifier--sentinel)
        (set-process-filter proc #'slack-notifier--filter)
        (set-process-coding-system proc 'utf-8 'utf-8)))))

(defun slack-notifier-kill ()
  "Kill the slack-notifier process."
  (interactive)
  (when (buffer-live-p slack-notifier--buffer)
    (kill-buffer slack-notifier--buffer)
    (run-hooks 'slack-notifier-kill-hook)))
