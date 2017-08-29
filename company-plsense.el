;; company-plsense.el --- -*- lexical-binding:t -*-

(require 'company)
(require 'cl-lib)
(require 'tq)
(require 'dash)
(require 's)

(defcustom company-plsense-executable "plsense"
  "the location of the PLSense executable. default is to search for it on $PATH")
(defcustom company-plsense-ignore-compile-errors t
  "Ignore errors from PLSense related to compiling libraries")
(defcustom company-plsense-config-path "~/.plsense"
  "The location of the plsense config file. Run 'plsense' from the shell to generate this file.")
(defcustom company-plsense-braces-autopaired t
  "wether or not to assume that braces are auto-paired")
(defcustom company-plsense-enabled-modes '(cperl-mode perl-mode)
  "major modes that will use plsense")

(defvar company-plsense--errors '())
(defvar company-plsense--queue nil)
(defvar company-plsense--process nil)
(defvar company-plsense--server-started-p nil)
(defvar company-plsense--error-flag nil)

(defvar company-plsense--current-file "")
(defvar company-plsense--current-package "")
(defvar company-plsense--current-method "")

(defvar company-plsense--done-re "\\`Done\\'")

(defun company-plsense--get-process ()
  (or (and (processp company-plsense--process)
           (eq (process-status (process-name company-plsense--process)) 'run)
           company-plsense--process)
      (company-plsense--start-process)))

(defun company-plsense--process-running-p ()
  (and (processp company-plsense--process)
       (eq (process-status (process-name company-plsense--process)) 'run)
       company-plsense--queue))

(defun company-plsense--process-exists-p ()
  (and (processp company-plsense--process)
       (process-status (process-name company-plsense--process))))

(defun company-plsense--server-request (cmd &optional callback)
  (unless (or company-plsense--server-started-p
           (company-plsense--process-running-p))
    (company-plsense--start-process))
  (company-plsense--async-request cmd (if callback
                                          (lambda (x resp)
                                            (funcall callback (replace-regexp-in-string "\n?>\\s-\\'" "" resp)))
                                        nil)))

(defun company-plsense--server-query (cmd &optional timeout)
  (unless (or company-plsense--server-started-p
              (company-plsense--process-running-p))
    (company-plsense--start-process))
  (company-plsense--sync-request cmd timeout))

(defun company-plsense--async-request (cmd callback)
  (tq-enqueue company-plsense--queue (if cmd (concat cmd "\n") "") ">\\s-\\'" nil callback t))

(defun company-plsense--sync-request (cmd &optional timeout)
  (let ((done nil)
        (reply "")
        (limit (if timeout (* 5 timeout) 25))
        (counter 0))
    (tq-enqueue company-plsense--queue
                (if cmd (concat cmd "\n") "")
                ">\\s-\\'"
                nil
                (lambda (x resp)
                  (setq reply resp)
                  (setq done t))
                t)
    (while (and (< counter limit) (not done))
      (accept-process-output company-plsense--process 0.2 nil t)
      (1+ counter))
    (if done
        (replace-regexp-in-string "\n?>\\s-\\'" "" reply)
      (setcar company-plsense--queue nil))))

(defun company-plsense--start-process ()
  (interactive)
  (if (not (file-exists-p company-plsense-config-path))
      (message "PlSense config file '%s' does not exist. run 'plsense' on shell to initalize it."
               (expand-file-name company-plsense-config-path))
    (message "Start plsense process.")
    (when (company-plsense--process-exists-p)
      (kill-process company-plsense--process)
      (sleep-for 1))
    (setq company-plsense--process
          (start-process-shell-command "plsense" nil (concat company-plsense-executable " --interactive")))
    (setq company-plsense--queue (company-plsense--tq-create company-plsense--process))
    (company-plsense--sync-request nil)
    (set-process-query-on-exit-flag company-plsense--process nil)
    company-plsense--queue))

(defun company-plsense-executable-version ()
  "Show PlSense Version."
  (interactive)
  (message (shell-command-to-string (concat company-plsense-executable " --version"))))

(defun company-plsense-server-status ()
  "Show status of server process."
  (interactive)
  (message (company-plsense--server-query "serverstatus" 10)))

(defun company-plsense-server-command (str)
  "Show status of server process."
  (interactive "Mcommand: ")
  (message (company-plsense--server-query str)))

(defun company-plsense--get-current-package ()
  (save-excursion
    ;; Current package may be starting line of package definition.
    (goto-char (point-at-eol))
    (or (when (re-search-backward "\\(?:^[[:space:]]*package[[:space:]]+\\([0-:A-Z_a-z]+\\)[[:space:]]*;\\)" nil t)
          (match-string-no-properties 1))
        "main")))

(defun company-plsense--open-file (fpath)
  (when (and
         (file-exists-p fpath)
         (not (file-directory-p fpath))
         (company-plsense--server-request
          (concat "open " (expand-file-name fpath))
          (lambda (resp)
            (when (string-match company-plsense--done-re resp)
              (setq company-plsense--current-file fpath) ;
              (setq company-plsense--current-package "main")
              (setq company-plsense--current-method "")
              (message "opened %s" fpath)))))))

(defun company-plsense-open-buffer ()
  (interactive)
  (company-plsense--open-file (buffer-file-name (current-buffer))))

;;;###autoload
(defun company-plsense-start-server ()
  "Start server process."
  (interactive)
  (message "Starting server ...")
  (company-plsense--server-request
   "serverstart"
   (lambda (resp)
     (setq company-plsense--server-started-p t)
     (when (string-match company-plsense--done-re resp)
       (message "plsense server started")))))

;;;###autoload
(defun company-plsense-stop-server ()
  "Stop server process."
  (interactive)
  (message "Stopping server ...")
  (company-plsense--sync-request "serverstop" 5)
  (company-plsense-kill-process)
  (message "plsense server stopped"))

(defun company-plsense-kill-process ()
  (interactive)
  (setq company-plsense--server-started-p nil)
  (tq-close company-plsense--queue))

;;;###autoload
(defun company-plsense-refresh-server ()
  "Refresh server process."
  (interactive)
  (company-plsense--server-request
   "refresh"
   (lambda (resp)
     (when (string-match company-plsense--done-re resp)
       (message "plsense server refreshed")))))

(defun company-plsense--update-file (file)
  (company-plsense--server-request
   (concat "update " file)
   (lambda (resp)
     (when (string-match company-plsense--done-re resp)
       (message "file updated")))))

(defun company-plsense-update ()
  "reanalyze the file"
  (interactive)
  (when (memq major-mode company-plsense-enabled-modes)
    (company-plsense--update-file (buffer-file-name (current-buffer)))))

(defun company-plsense-setup ()
  (interactive)
  (unless company-plsense--server-started-p (company-plsense-start-server))
  (--each company-plsense-enabled-modes
    (add-hook (intern-soft (concat (symbol-name it) "-hook")) 'company-mode))
  ;; (add-hook 'cperl-mode-hook 'company-mode)
  (add-hook 'after-save-hook 'company-plsense-update)
  (add-to-list 'company-backends 'company-plsense))

(defun company-plsense-update-location ()
  "set the current file and module for the server."
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
    (unless (string= company-plsense--current-file file)
      (company-plsense--server-request (concat "onfile " file))
      (setq company-plsense--current-file file)
      (let ((pkg (company-plsense--get-current-package)))
        (company-plsense--server-request (concat "onmod " pkg))
        (setq company-plsense--current-package pkg)))))

(defun company-plsense-prefix ()
  "Grab prefix at point.
Properly detect strings, comments and variables."
  (when (memq major-mode company-plsense-enabled-modes)
    (company-plsense-update-location)
    (unless (company-in-string-or-comment)
      (--if-let (when (or (looking-back "[$@%&[:alnum:]_{([]" (- (point) 1))
                          (looking-back "\\(?:->\\|::\\|{^\\|$^\\)" (- (point) 2)))
                  (save-match-data
                    (let ((line (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (point))))
                      (if (string-match "\\(?:[$@%]{?^?\\|[a-zA-Z_]\\)[[:alnum:]_:]*\\'" line)
                          (match-string-no-properties 0 line)
                        ""))))
          (if (looking-back "[>$@%&:{([].*" (- (point) company-minimum-prefix-length))
              (cons it t)
            it)
        'stop))))

(defun company-plsense-candidates (callback prefix)
  (let ((static-prefix (replace-regexp-in-string "[^$@%]*\\'" "" prefix))
        (buffer (current-buffer))
        (line (buffer-substring-no-properties
               (line-beginning-position)
               (point))))
    (company-plsense--server-request
     (concat "assist " line)
     (lambda (resp)
       (with-current-buffer buffer
         (funcall
          callback
          (--map (concat static-prefix it)
                 (s-split "\n" resp t))))))))

(defun company-plsense-doc-buffer (candidate)
  (company-doc-buffer
   (company-plsense--server-query
    (concat "assisthelp " (s-chop-prefixes '("$" "@" "%") candidate)))))

;;;###autoload
(defun company-plsense (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-plsense))
    (init (company-plsense--open-file (buffer-file-name (current-buffer))))
    (prefix (company-plsense-prefix))
    (candidates (cons :async
                      (lambda (callback)
                        (company-plsense-candidates callback arg))))
    (doc-buffer (company-plsense-doc-buffer arg))
    (post-completion (when (and company-plsense-braces-autopaired
                                (s-suffix? "}" arg)
                                (looking-at "}"))
                       (delete-char 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; modified transaction queue
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the `tq' library will throw an error when input it recieved and there is not
;; outstanding server call. Plsense will raise errors asyncrounously and this will
;; cause the queue to fail. These overrided methods all us to handle spurious
;; input.

(defun company-plsense--tq-create (process)
  "Create and return a transaction queue communicating with PROCESS.
PROCESS should be a subprocess capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine."
  (let ((tq (cons nil (cons process
                            (generate-new-buffer
                             (concat " tq-temp-"
                                     (process-name process)))))))
    (buffer-disable-undo (tq-buffer tq))
    (set-process-filter process
                        (lambda (_proc string) (company-plsense--tq-filter tq string)))
    tq))

(defun company-plsense--tq-filter (tq string)
  "Append STRING to the COMPANY-PLSENSE--TQ's buffer; then process the new data."
  (let ((buffer (tq-buffer tq))
        (reply (company-plsense--handle-errors string)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert reply)
        (company-plsense--tq-process-buffer tq)))))

(defun company-plsense--tq-process-buffer (tq)
  "Check COMPANY-PLSENSE--TQ's buffer for the regexp at the head of the queue."
  (let ((buffer (tq-buffer tq)))
    (when (buffer-live-p buffer)
      (set-buffer buffer)
      (when (/= 0 (buffer-size))
        (if (tq-queue-empty tq)
            (delete-region (point-min) (point-max))
          (goto-char (point-min))
          (if (re-search-forward (tq-queue-head-regexp tq) nil t)
              (let ((answer (buffer-substring (point-min) (point))))
                (delete-region (point-min) (point))
                (unwind-protect
                    (condition-case nil
                        (funcall (tq-queue-head-fn tq)
                                 (tq-queue-head-closure tq)
                                 answer)
                      (error nil))
                  (tq-queue-pop tq))
                (company-plsense--tq-process-buffer tq))))))))

(defun company-plsense--handle-errors (msg)
  (let ((error-re "ERROR: .*\n"))
    (save-match-data
      (let ((pos 0))
        (while (string-match error-re msg pos)
          (let ((error (match-string 0 msg)))
            (unless (and company-plsense-ignore-compile-errors
                         (string-prefix-p "ERROR: Failed compile" error))
              (setq company-plsense--error-flag t)
              (message "plsense server: " error)
              (with-current-buffer (get-buffer-create "*company-plsense-errors*")
                (goto-char (point-max))
                (insert error))))
          (setq pos (match-end 0)))))
    (replace-regexp-in-string error-re "" msg)))

(provide 'company-plsense)
