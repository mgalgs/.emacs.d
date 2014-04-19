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
