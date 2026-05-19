;;; init-dashboard.el --- Minimal project dashboard  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar m/dashboard-projects-file
  (expand-file-name "dashboard-projects" user-emacs-directory)
  "File storing the persisted list of project directories (most recent first).")

(defvar m/dashboard-projects nil
  "List of project directories, most recent first.")

(defvar m/dashboard-max-projects 30
  "Maximum number of projects to track.")

(defvar m/dashboard-buffer-name "*Dashboard*")

;;; --- Persistence ---

(defun m/dashboard--load-projects ()
  "Load projects from `m/dashboard-projects-file'."
  (when (file-exists-p m/dashboard-projects-file)
    (setq m/dashboard-projects
          (with-temp-buffer
            (insert-file-contents m/dashboard-projects-file)
            (read (current-buffer))))))

(defun m/dashboard--save-projects ()
  "Save projects to `m/dashboard-projects-file'."
  (with-temp-file m/dashboard-projects-file
    (prin1 m/dashboard-projects (current-buffer))))

;;; --- Project tracking ---

(defun m/dashboard-record-project ()
  "Record `default-directory' as a recent project.
Intended for use in `magit-status-mode-hook'."
  (let ((dir (abbreviate-file-name default-directory)))
    (setq m/dashboard-projects
          (cons dir (delete dir m/dashboard-projects)))
    (when (> (length m/dashboard-projects) m/dashboard-max-projects)
      (setcdr (nthcdr (1- m/dashboard-max-projects) m/dashboard-projects) nil))
    (m/dashboard--save-projects)))

(add-hook 'magit-status-mode-hook #'m/dashboard-record-project)

;;; --- Dashboard buffer ---

(defvar m/dashboard-mode-map (make-sparse-keymap)
  "Keymap for `m/dashboard-mode'.")

(define-derived-mode m/dashboard-mode special-mode "Dashboard"
  "Major mode for the project dashboard."
  (setq buffer-read-only t
        truncate-lines t))

(defun m/dashboard-next ()
  "Move to the next project line."
  (interactive)
  (forward-line 1)
  (when (eobp) (forward-line -1))
  (beginning-of-line))

(defun m/dashboard-prev ()
  "Move to the previous project line."
  (interactive)
  (let ((prev (line-beginning-position)))
    (forward-line -1)
    (unless (m/dashboard--project-on-line)
      (goto-char prev))
    (beginning-of-line)))

(defvar-local m/dashboard--last-project nil
  "The last project directory point was on, used to restore position.")
(put 'm/dashboard--last-project 'permanent-local t)

(defvar-local m/dashboard--filter nil
  "Current filter string, or nil for no filter.")
(put 'm/dashboard--filter 'permanent-local t)

(defun m/dashboard--project-on-line ()
  "Return the project directory for the current line, or nil."
  (get-text-property (line-beginning-position) 'dashboard-project))

(defun m/dashboard-open-project ()
  "Open magit-status for the project on the current line."
  (interactive)
  (when-let ((dir (m/dashboard--project-on-line)))
    (setq m/dashboard--last-project dir)
    (if (file-directory-p dir)
        (magit-status dir)
      (message "Directory no longer exists: %s" dir))))

(defun m/dashboard-remove-project ()
  "Remove the project on the current line from the list."
  (interactive)
  (when-let ((dir (m/dashboard--project-on-line)))
    (setq m/dashboard-projects (delete dir m/dashboard-projects))
    (m/dashboard--save-projects)
    (m/dashboard-refresh)))

(defun m/dashboard-refresh ()
  "Refresh the dashboard buffer and clear any active filter."
  (interactive)
  (setq m/dashboard--filter nil)
  (m/dashboard--render))

(defun m/dashboard--filter-regexp (filter)
  "Build a regexp from FILTER that matches all space-separated tokens in order."
  (mapconcat #'regexp-quote (split-string (downcase filter)) ".*"))

(defun m/dashboard--filter-do-in-window (dashboard-buf func)
  "Call FUNC in the window displaying DASHBOARD-BUF."
  (let ((win (get-buffer-window dashboard-buf)))
    (when win (with-selected-window win (funcall func)))))

(defun m/dashboard-filter ()
  "Interactively filter the project list.
Type to narrow results live.  C-n/C-p to navigate results.
C-j to open the selected project.  RET accepts the filter.
C-g cancels and restores the full list."
  (interactive)
  (let ((dashboard-buf (current-buffer))
        (accepted nil)
        (filter-map (make-sparse-keymap)))
    (set-keymap-parent filter-map minibuffer-local-map)
    (define-key filter-map (kbd "C-n")
      (lambda () (interactive)
        (m/dashboard--filter-do-in-window dashboard-buf #'m/dashboard-next)))
    (define-key filter-map (kbd "C-p")
      (lambda () (interactive)
        (m/dashboard--filter-do-in-window dashboard-buf #'m/dashboard-prev)))
    (define-key filter-map (kbd "C-j")
      (lambda () (interactive)
        (with-current-buffer dashboard-buf
          (setq m/dashboard--filter nil)
          (m/dashboard-open-project))
        (abort-recursive-edit)))
    (setq m/dashboard--filter nil)
    (m/dashboard--render)
    (let ((prev-input ""))
      (unwind-protect
          (progn
            (minibuffer-with-setup-hook
                (lambda ()
                  (add-hook 'post-command-hook
                            (lambda ()
                              (let ((input (minibuffer-contents)))
                                (unless (equal input prev-input)
                                  (setq prev-input input)
                                  (m/dashboard--filter-do-in-window
                                   dashboard-buf
                                   (lambda ()
                                     (setq m/dashboard--filter
                                           (unless (string-empty-p input) input))
                                     (m/dashboard--render))))))
                            nil t))
            (read-from-minibuffer "Filter: " nil filter-map))
          (setq accepted t)
          (goto-char (point-min))
          (forward-line 2)
          (beginning-of-line))
        (unless accepted
          (when (buffer-live-p dashboard-buf)
            (with-current-buffer dashboard-buf
              (setq m/dashboard--filter nil)
              (m/dashboard--render))))))))

(defun m/dashboard--render ()
  "Render the dashboard in the current buffer."
  (let ((inhibit-read-only t)
        (prev-project (or (m/dashboard--project-on-line)
                          m/dashboard--last-project)))
    (erase-buffer)
    (m/dashboard-mode)
    (setq m/dashboard--last-project prev-project)
    (insert (propertize "Projects" 'face 'bold)
            (if m/dashboard--filter
                (propertize (concat "  [" m/dashboard--filter "]")
                            'face 'font-lock-string-face)
              "")
            "\n\n")
    (let ((projects (if m/dashboard--filter
                       (let ((re (m/dashboard--filter-regexp m/dashboard--filter)))
                         (cl-remove-if-not
                          (lambda (dir)
                            (string-match-p re (downcase dir)))
                          m/dashboard-projects))
                     m/dashboard-projects)))
      (if (null projects)
          (insert (propertize (if m/dashboard--filter
                                  "  No matching projects."
                                "  No projects yet. Open magit-status in a repo to add one.")
                              'face 'font-lock-comment-face))
        (dolist (dir projects)
          (let ((name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
                (start (point)))
            (insert (propertize (concat "  " name) 'face 'font-lock-keyword-face)
                    (propertize (concat "  " dir) 'face 'font-lock-comment-face)
                    "\n")
            (put-text-property start (point) 'dashboard-project dir)))))
    (goto-char (point-min))
    (forward-line 2)
    (when prev-project
      (let ((found nil))
        (save-excursion
          (goto-char (point-min))
          (while (and (not found) (not (eobp)))
            (when (equal (m/dashboard--project-on-line) prev-project)
              (setq found (point)))
            (forward-line 1)))
        (when found (goto-char found))))
    (beginning-of-line)))

;;;###autoload
(defun m/dashboard ()
  "Show the project dashboard."
  (interactive)
  (m/dashboard--load-projects)
  (switch-to-buffer (get-buffer-create m/dashboard-buffer-name))
  (m/dashboard--render))

;; Load persisted projects at init time.
(m/dashboard--load-projects)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
