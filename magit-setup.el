(require 'magit)

(defun my-git-nrevs-gate (nrevs)
  "Returns `nrevs' or `git rev-parse --max-count=num HEAD | wc -l',
whichever is less"
  (let* ((have-revs (string-to-number (shell-command-to-string (format "git rev-list --max-count=%s HEAD | wc -l"
                                                                       nrevs)))))
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
  (format "%s.."
          (my-git-get-rev-nrevs-back n)))

;; (setq magit-log-default-log-range 'my-git-get-log-range)
(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-status-buffer-switch-function 'switch-to-buffer)

(defun my-magit-insert-recent-log ()
  (magit-git-insert-section (recent "Recent log:")
      (lambda ()
        (ansi-color-apply-on-region (point-min) (point-max))
        ;; (insert (buffer-string))
        )
    ;; (apply-partially 'magit-wash-log 'oneline 'color t)
    ;; (lambda () (message "Recent'ing") (insert (buffer-string)))
    ;; (apply-partially 'magit-wash-log 'unique)
    "log"
    ;; "--decorate=full"
    ;; "--color"
    "--graph"
    "--oneline"
    "--decorate"
    "--color"
    ;; (concat "--pretty=format:%h%d "
    ;;         (and magit-log-show-gpg-status "%G?")
    ;;         "[%an][%at]%s")
    (my-git-get-log-range 20)))

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
