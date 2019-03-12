(setq magit-last-seen-setup-instructions "1.4.0")

;; avoid conflicts with vc:
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-push-always-verify nil)     ; cuz it says so

(defun m/magit-display-buffer-traditional (buffer)
  "Like magit-display-buffer-traditional, but re-uses window for status mode, too."
  (display-buffer
   buffer (if (not (memq (with-current-buffer buffer major-mode)
                         '(magit-process-mode
                           magit-revision-mode
                           magit-diff-mode
                           magit-stash-mode)))
              '(display-buffer-same-window)
            nil)))

(setq magit-display-buffer-function 'm/magit-display-buffer-traditional)

(add-to-list 'git-commit-known-pseudo-headers "Change-Id")
(add-to-list 'git-commit-known-pseudo-headers "CRs-Fixed")
(add-to-list 'git-commit-known-pseudo-headers "Git-commit")
(add-to-list 'git-commit-known-pseudo-headers "Git-repo")
(add-to-list 'git-commit-known-pseudo-headers "Fixes")
(add-to-list 'git-commit-known-pseudo-headers "Tested-by")

(setq magit-log-section-commit-count' 10)
(setq magit-log-section-arguments '("--graph" "--decorate" "--color"))

(setq magit-status-sections-hook
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-unpushed-cherries
        magit-insert-recent-commits
        magit-insert-stashes
        magit-insert-unpulled-from-pushremote
        magit-insert-unpushed-to-upstream
        magit-insert-unpushed-to-pushremote
        magit-insert-unpulled-from-upstream))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)

(defun m/magit-reset-author (&optional args)
  "Resets the authorship information for the last commit"
  (interactive)
  (magit-run-git-async "commit" "--amend" "--no-edit" "--reset-author"))

;; (magit-define-popup-action 'magit-commit-popup
;;   ?R "Reset author" 'm/magit-reset-author)
(transient-append-suffix 'magit-commit
  "S"
  '("R" "Reset author" m/magit-reset-author))

(setq git-commit-finish-query-functions nil)
