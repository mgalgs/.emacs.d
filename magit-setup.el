(require 'magit)

(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

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

(setq magit-push-hook 'magit-push-dwis)

(setq magit-status-sections-hook
      '(magit-insert-status-local-line
        magit-insert-status-remote-line
        magit-insert-status-head-line
        magit-insert-status-tags-line
        magit-insert-status-merge-line
        magit-insert-status-rebase-lines
        magit-insert-empty-line
        magit-insert-pending-commits
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-untracked-files
        magit-insert-unpushed-cherries
        magit-insert-unpulled-cherries
        magit-insert-recent-commits-graph
        magit-insert-stashes))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)
