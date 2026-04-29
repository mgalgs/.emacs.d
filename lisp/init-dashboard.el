;;; init-dashboard.el --- Minimal project dashboard  -*- lexical-binding: t; -*-

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

(defvar m/dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'m/dashboard-next)
    (define-key map (kbd "p") #'m/dashboard-prev)
    (define-key map (kbd "RET") #'m/dashboard-open-project)
    (define-key map (kbd "g") #'m/dashboard-refresh)
    (define-key map (kbd "d") #'m/dashboard-remove-project)
    map)
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
  (forward-line -1)
  (beginning-of-line))

(defun m/dashboard--project-on-line ()
  "Return the project directory for the current line, or nil."
  (get-text-property (line-beginning-position) 'dashboard-project))

(defun m/dashboard-open-project ()
  "Open magit-status for the project on the current line."
  (interactive)
  (when-let ((dir (m/dashboard--project-on-line)))
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
  "Refresh the dashboard buffer."
  (interactive)
  (m/dashboard--render))

(defun m/dashboard--render ()
  "Render the dashboard into `m/dashboard-buffer-name'."
  (let ((buf (get-buffer-create m/dashboard-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (line (line-number-at-pos)))
        (erase-buffer)
        (m/dashboard-mode)
        (insert (propertize "Projects" 'face 'bold) "\n\n")
        (if (null m/dashboard-projects)
            (insert (propertize "  No projects yet. Open magit-status in a repo to add one."
                                'face 'font-lock-comment-face))
          (dolist (dir m/dashboard-projects)
            (let ((name (file-name-nondirectory (directory-file-name (expand-file-name dir))))
                  (start (point)))
              (insert (propertize (concat "  " name) 'face 'font-lock-keyword-face)
                      (propertize (concat "  " dir) 'face 'font-lock-comment-face)
                      "\n")
              (put-text-property start (point) 'dashboard-project dir))))
        ;; Restore cursor position, defaulting to first project line.
        (goto-char (point-min))
        (let ((target (if (= line 1) 3 line)))
          (forward-line (1- (min target (count-lines (point-min) (point-max))))))
        (beginning-of-line)))
    buf))

;;;###autoload
(defun m/dashboard ()
  "Show the project dashboard."
  (interactive)
  (m/dashboard--load-projects)
  (switch-to-buffer (m/dashboard--render)))

;; Load persisted projects at init time.
(m/dashboard--load-projects)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
