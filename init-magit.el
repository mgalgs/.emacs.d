(setq magit-last-seen-setup-instructions "1.4.0")

;; avoid conflicts with vc:
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-push-always-verify nil)     ; cuz it says so

(add-to-list 'git-commit-known-pseudo-headers "Change-Id")
(add-to-list 'git-commit-known-pseudo-headers "CRs-Fixed")
(add-to-list 'git-commit-known-pseudo-headers "Git-commit")
(add-to-list 'git-commit-known-pseudo-headers "Git-repo")
(add-to-list 'git-commit-known-pseudo-headers "Fixes")
(add-to-list 'git-commit-known-pseudo-headers "Tested-by")

(setq magit-log-section-commit-count' 15)
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
        magit-insert-unpulled-commits
        magit-insert-recent-commits
        magit-insert-stashes))

(defun m/magit-file-log ()
  (interactive)
  (magit-log '("HEAD") nil (list (magit-file-relative-name))))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)
