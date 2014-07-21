(require 'magit)

(setq gthings-completing-read 'ido-completing-read)
(setq gthings-git-diff-buffer-name "*gthings-diff*")
(setq gthings-git-show-buffer-name-fmt "*gthings-show-at-%s*")


(defun gthings-run-git-cmd (cmd output-buffer-name)
  "runs a git command `CMD', placing output in
`OUTPUT-BUFFER-NAME'."
  (let ((current-major-mode major-mode)
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
    (funcall current-major-mode)
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


(defun gthings-diff-buffer-against-branch (&optional only-current-buffer)
  "Diff the current buffer against another branch in git. With
prefix, only diffs the current buffer."
  (interactive "P")
    (let* ((the-branches-command-output (shell-command-to-string (concat "git branch")))
	   (the-branches (delete "" (split-string the-branches-command-output "\n")))
	   (the-cleaned-branches (mapcar (lambda (branch)
						(substring branch 2)) the-branches))
	   (selected-branch (ido-completing-read "Branch: "
						     the-cleaned-branches)))
      (gthings-git-diff selected-branch only-current-buffer)))

(defun gthings-show-file-at-rev (&optional rev file)
  "Shows the current file at revision in a new buffer as it"
  (interactive)
  (let* ((rev (if rev rev
		(magit-read-rev "Revision")))
	 (show-cmd (format "show \"%s:./%s\""
			   rev
			   (if file
                               file
                             (if current-prefix-arg
                                 (file-relative-name (read-file-name "File: ")
                                                     default-directory)
                               (file-name-nondirectory (buffer-file-name)))))))
    (gthings-run-git-cmd show-cmd
			 (format gthings-git-show-buffer-name-fmt rev))))

(provide 'gthings)
