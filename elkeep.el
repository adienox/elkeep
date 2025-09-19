;;; elkeep.el --- Google Keep for Emacs -*- lexical-binding: t; -*-

;; Author: Adwait Adhikari
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0"))
;; Keywords: convenience, org, roam, google keep
;; URL: https://github.com/adienox/elkeep

;;; Commentary:
;; This package creates a bridge between google keep and Emacs.

;;; Code:
(require 'org-roam)
(require 'org-roam-dailies)
(require 'bookmark)

(defgroup elkeep nil
  "Integration with elkeep-cli for note management."
  :group 'applications)

(defcustom elkeep-cli-bin (or (executable-find "elkeep-cli") "elkeep-cli")
  "Path to the elkeep-cli binary.
Defaults to whatever `executable-find' locates in your PATH, or
just `elkeep-cli' if nothing is found."
  :type 'string
  :group 'elkeep)

(defcustom elkeep-notes-directory nil
  "Directory where elkeep saves notes.
If you use Org-roam, you may want to set this to `org-roam-directory`."
  :type 'directory
  :group 'elkeep)

(defcustom elkeep-journal-directory nil
  "Directory where elkeep saves journals.
If you use Org-roam dailies, you may want to set this to
  (expand-file-name org-roam-dailies-directory org-roam-directory)."
  :type 'directory
  :group 'elkeep)

(defun elkeep-run-cli-async (args buffer-name sentinel)
  "Run `elkeep-cli' with ARGS asynchronously in BUFFER-NAME.
Call SENTINEL when process changes state."
  (make-process
   :name buffer-name
   :buffer buffer-name
   :command (cons elkeep-cli-bin args)
   :noquery t
   :sentinel sentinel))

(defun elkeep-parse-json-buffer (buffer)
  "Return list of entries from BUFFER containing JSON output.
Handles extra lines and trims whitespace."
  (with-current-buffer buffer
    (let* ((json-lines (split-string (buffer-string) "\n" t))
           (json-clean (seq-find (lambda (line)
                                   (string-match-p "\\`\\s-*\\[" line))
                                 json-lines)))
      (when json-clean
        (json-parse-string (string-trim json-clean)
                           :false-object nil
                           :null-object nil
                           :object-type 'alist
                           :array-type 'list)))))

(defun elkeep-prompt-for-entry (entries)
  "Prompt user to choose an entry from ENTRIES.
Returns cons (id . notitle) of the chosen entry."
  (let* ((candidates (mapcar (lambda (entry)
                               (cons (alist-get 'name entry)
                                     (cons (alist-get 'id entry) (alist-get 'notitle entry))))
                             entries))
         (choice (completing-read "Choose entry: " (mapcar #'car candidates))))
    (message "%s" (cdr (assoc choice candidates)))
    (cdr (assoc choice candidates))))

(defun elkeep-get-entries-sentinel (interactive-p proc event)
  "Sentinel for `elkeep-get-entries'.
INTERACTIVE-P indicates if function was called interactively.
`PROC' is the process, `EVENT' is the event string."
  (cond
   ;; Success
   ((and (eq (process-status proc) 'exit)
         (= (process-exit-status proc) 0))
    (let ((entries (elkeep-parse-json-buffer (process-buffer proc))))
      (if entries
          (let* ((prop (elkeep-prompt-for-entry entries))
                 (id   (car prop))
                 (notitle (cdr prop)))
            (when interactive-p
              (if notitle
                  (progn
                    (let* ((title (nano-read-with-info "TITLE:" "Note is without a title")))
                      (elkeep-save-entry id title)))
                (elkeep-save-entry id)))
            id)
        (message "No valid JSON entries found.")))
    (kill-buffer (process-buffer proc)))

   ;; Error
   ((eq (process-status proc) 'exit)
    (message "elkeep-cli -l failed (exit %d). See buffer %s"
             (process-exit-status proc)
             (buffer-name (process-buffer proc))))))

(defun elkeep-save-entry (id &optional title)
  "Download a given entry with `ID' with optional `TITLE'.
Then sync org-roam DB if successful.
On failure, display the process buffer for debugging."
  (message "Downloading note...")
  (make-process
   :name "elkeep-get-entry"
   :buffer "*elkeep-get-entry*"
   :command (append
             (list elkeep-cli-bin "-g" id "-o" elkeep-notes-directory)
             (when title (list "-T" title)))
   :noquery t
   :sentinel
   (lambda (process event)
     (cond
      ;; Success
      ((and (eq (process-status process) 'exit)
            (= (process-exit-status process) 0))

       ;; Add file to bookmark
       (with-current-buffer (process-buffer process)
         (let* ((output (string-trim (buffer-string)))
                (saved-file-path (cadr (split-string output ": ")))
                (bm-name "elkeep-last-downloaded"))
           (when saved-file-path
             (with-current-buffer (find-file-noselect saved-file-path)
               (bookmark-set bm-name)))))

       (message "Downloaded entry %s, syncing Org-roam DB..." id)
       (org-roam-db-sync)

       ;; kill buffer to avoid clutter
       (kill-buffer (process-buffer process)))

      ;; Error
      ((eq (process-status process) 'exit)
       (message "Download failed (exit %d). See buffer %s"
                (process-exit-status process) (buffer-name (process-buffer process))))))))

;;;###autoload
(defun elkeep-get-entries ()
  "Retrieve entries from `elkeep-cli' and prompt user.
If called interactively, automatically save the chosen entry."
  (interactive)
  (let ((interactive-p (called-interactively-p 'any)))
    (message "Querying keep for notes...")
    (elkeep-run-cli-async
     '("-l")
     "*elkeep-cli-output*"
     (lambda (proc event)
       (elkeep-get-entries-sentinel interactive-p proc event)))))

;;;###autoload
(defun elkeep-save-journal ()
  "Retrieve journal from `elkeep-cli' and save to `elkeep-journal-directory'."
  (interactive)
  (elkeep-run-cli-async
   `("-j" ,elkeep-journal-directory)
   "*elkeep-cli-output*"
   (lambda (process event)
     (cond
      ;; Success
      ((and (eq (process-status process) 'exit)
            (= (process-exit-status process) 0))
       (message "Journal download finished.")
       (org-roam-db-sync)

       ;; kill buffer to avoid clutter
       (kill-buffer (process-buffer process)))

      ;; Error
      ((eq (process-status process) 'exit)
       (message "elkeep-cli -j failed (exit %d). See buffer %s"
                (process-exit-status process)
                (buffer-name (process-buffer process))))))))

(provide 'elkeep)
;;; elkeep.el ends here
