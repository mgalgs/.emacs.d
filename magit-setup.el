(require 'magit)
(require 'rebase-mode)

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

(defun my-git-get-log-range ()
  "Get something like HEAD~100.. (with the `100' sanitized to
only go back as far as the git history) to pass to magit-log."
  (format "%s.."
          (my-git-get-rev-nrevs-back 100)))

(setq magit-log-default-log-range 'my-git-get-log-range)
