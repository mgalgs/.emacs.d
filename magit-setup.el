(require 'magit)

(defun my-git-nrevs (max-count)
  (1- (length (split-string (shell-command-to-string (format "git rev-list --max-count=%s HEAD"
                                                             max-count))
                            "\n"))))

(defun my-git-nrevs-gate (nrevs)
  "Returns `nrevs' or `git rev-parse --max-count=num HEAD | wc -l',
whichever is less. This is useful to prevent oneself from falling
off the beginning of a git history."
  (let* ((have-revs (my-git-nrevs nrevs)))
    (min nrevs have-revs)))

(defun my-git-get-rev-nrevs-back (nrevs)
  "Returns the ref back `nrevs' (or less if so determined by
`my-git-nrevs-gate')"
  (substring (shell-command-to-string (format "git rev-parse HEAD~%d"
                                              (1- (my-git-nrevs-gate nrevs))))
             0 -1))

(defun my-git-get-log-range (n)
  "Get something like HEAD~n.. (with the `n' sanitized to
only go back as far as the git history) to pass to magit-log."
  (if (= n (my-git-nrevs n))
      ;; there's at least n commits. Let's limit with .. for
      ;; performance.
      (format "%s.." (my-git-get-rev-nrevs-back n))
    "HEAD"))

;; (setq magit-log-default-log-range 'my-git-get-log-range)
(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(defun my-magit-insert-recent-log ()
  ;; doesn't work if there aren't any commits yet
  (when (= 0 (call-process "git" nil nil nil "rev-parse" "HEAD"))
    (magit-git-insert-section (recent "Recent log:")
        (lambda ()
          (ansi-color-apply-on-region (point-min) (point-max)))
      "log"
      "--graph"
      "--oneline"
      "--decorate"
      "--color"
      (my-git-get-log-range 15)
      (format "--max-count=%d" 15))))

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
        my-magit-insert-recent-log
        magit-insert-stashes))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)
