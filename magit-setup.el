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

(setq my-magit-recent-log-max-count 15)

(defun my-magit-insert-recent-commits-graph ()
  ;; doesn't work if there aren't any commits yet. If `git rev-parse HEAD'
  ;; fails then there aren't any commits.
  (when (= 0 (call-process "git" nil nil nil "rev-parse" "HEAD"))
    (let ((revs (magit-git-lines "rev-list"
                                 (format "--max-count=%d" (1+ my-magit-recent-log-max-count))
                                 "HEAD")))
      (magit-git-insert-section (recent "Recent commits:")
          (lambda ()
            (ansi-color-apply-on-region (point-min) (point-max)))
        "log"
        "--graph"
        "--oneline"
        "--decorate"
        "--color"
        (if (> (length revs) my-magit-recent-log-max-count)
            ;; here we are. the reason we go through all this
            ;; `rev-list' effort is for this range. This results in a
            ;; dramatic performance improvement over --graph
            ;; --max-count for repos with lots of commits.
            (format "%s.." (car (last revs)))
          "HEAD")
        (format "--max-count=%d" my-magit-recent-log-max-count)))))

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
        my-magit-insert-recent-commits-graph
        magit-insert-stashes))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)
