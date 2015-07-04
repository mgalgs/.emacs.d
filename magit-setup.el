(setq magit-last-seen-setup-instructions "1.4.0")

(require 'magit)

(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(add-to-list 'git-commit-known-pseudo-headers "Change-Id")
(add-to-list 'git-commit-known-pseudo-headers "CRs-Fixed")
(add-to-list 'git-commit-known-pseudo-headers "Git-commit")
(add-to-list 'git-commit-known-pseudo-headers "Git-repo")
(add-to-list 'git-commit-known-pseudo-headers "Fixes")
(add-to-list 'git-commit-known-pseudo-headers "Tested-by")

(defun magit-push-dwis (arg)
  "Like `magit-push-dwim' but doesn't mess with setting upstream
branches or push to branch.<name>.merge by default. The goal here
is to respect the config push.default. If push.default=current
you really want to push to the remote branch of the same name as
the local branch, even if your
upstream (i.e. branch.<name>.merge) is set to something else."
  (interactive "P")
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "Don't push a detached head.  That's gross")))
         (auto-remote (magit-get-remote branch))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote))
         (used-branch (when (>= (prefix-numeric-value arg) 16)
                        (magit-read-remote-branch
                         (format "Push %s as branch" branch)
                         used-remote))))
    (magit-run-git-async
     "push" "-v" used-remote
     (if used-branch (format "%s:%s" branch used-branch) branch)
     magit-custom-options)))

;; (setq magit-push-hook 'magit-push-dwis)

(setq magit-log-section-commit-count' 15)

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
        magit-insert-unpushed-commits
        magit-insert-unpulled-commits
        magit-insert-recent-commits-graph
        magit-insert-stashes))

(defun my-magit-file-log ()
  (interactive)
  (magit-log '("HEAD") nil (list (magit-file-relative-name))))

;; (setq magit-status-sections-hook
;;       (append magit-status-sections-hook
;;               '(magit-insert-recent-commits)))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)
