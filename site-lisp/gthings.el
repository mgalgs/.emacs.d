(provide 'gthings)

(setq gthings-completing-read 'ido-completing-read)
(setq gthings-git-diff-buffer-name "*gthings-diff*")


;; run git diff and put it in a special diff buffer
(defun gthings-git-diff (diff-args &optional current-buffer-only)
  (let* ((the-diff-args (if current-buffer-only
			    (concat diff-args " -- " (buffer-file-name))
			  diff-args))
	 (diff-cmd (concat "diff " the-diff-args))
	 (old-buffer (get-buffer gthings-git-diff-buffer-name)))
    (message "Running git %s" diff-cmd)
    (if old-buffer
	(kill-buffer old-buffer))
    (apply 'call-process
	   "git"
	   nil
	   gthings-git-diff-buffer-name
	   t
	   (split-string diff-cmd " "))
    (switch-to-buffer gthings-git-diff-buffer-name)
    (goto-char (point-min))
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

