(require 'magit)

(setq gthings-completing-read 'ido-completing-read)
(setq gthings-git-diff-buffer-name "*gthings-diff*")
(setq gthings-git-show-buffer-name-fmt "*gthings-show-at-%s*")

(defun gthings-run-git-cmd (cmd output-buffer-name &optional original-filename)
  "Runs a git command `CMD', placing output in
`OUTPUT-BUFFER-NAME', and enabling the mode for `ORIGINAL-FILENAME'."
  (let* ((current-major-mode (if original-filename
                                 (assoc-default original-filename auto-mode-alist
                                                'string-match)
                               major-mode))
         (old-buffer (get-buffer output-buffer-name)))
    (if old-buffer
        (kill-buffer old-buffer))
    (message "Running gthings git command: %s" cmd)
    (apply 'call-process
           "git"
           nil
           output-buffer-name
           t
           (split-string-and-unquote cmd " "))
    (switch-to-buffer output-buffer-name)
    (when current-major-mode
      (funcall current-major-mode))
    (setq buffer-read-only t)
    (view-mode)
    (goto-char (point-min))))

;; run git diff and put it in a special diff buffer
(defun gthings-git-diff (diff-args &optional current-buffer-only)
  (let* ((the-diff-args (if current-buffer-only
			    (concat diff-args " -- " (buffer-file-name))
			  diff-args))
	 (diff-cmd (concat "diff " the-diff-args))
	 (old-buffer (get-buffer gthings-git-diff-buffer-name)))
    (gthings-run-git-cmd diff-cmd gthings-git-diff-buffer-name)
    (diff-mode)))


(defun gthings-diff-buffer-against-branch ()
  "Diff the current buffer against another branch in git."
  (interactive)
  (let* ((the-branches-command-output (shell-command-to-string (concat "git branch")))
         (the-branches (delete "" (split-string the-branches-command-output "\n")))
         (the-cleaned-branches (mapcar (lambda (branch)
                                         (substring branch 2)) the-branches))
         (selected-branch (ido-completing-read "Branch: "
                                               the-cleaned-branches)))
    (gthings-git-diff selected-branch t)))

(defun gthings-show-file-at-rev (&optional rev file)
  "Shows the current file at revision in a new buffer. With a prefix
arg, also prompts for the filename (completion populated by `git
ls-files` at the specified revision)."
  (interactive)
  (let* ((rev (if rev rev
                (magit-read-branch-or-commit "Revision")))
         (file-list-cmd (format "git ls-tree --name-only -r %s" rev))
         (file-list (split-string (shell-command-to-string file-list-cmd)
                                  "\n" t))
         (filename (if (or current-prefix-arg (not (buffer-file-name)))
                   (ido-completing-read "File: " file-list)
                 (if filename
                     filename
                   (file-name-nondirectory (buffer-file-name))))))
    (let ((show-cmd (format "show \"%s:./%s\"" rev filename)))
      (gthings-run-git-cmd show-cmd
                           (format gthings-git-show-buffer-name-fmt rev)
                           filename))))

(provide 'gthings)
