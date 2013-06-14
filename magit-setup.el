(require 'magit)
(require 'rebase-mode)

(defun my-git-nrevs-gate (nrevs)
  "Returns `nrevs' or `git rev-parse --max-count=num HEAD | wc -l',
whichever is less"
  (let* ((have-revs (string-to-number (shell-command-to-string (format "git rev-list --max-count=%s HEAD | wc -l"
                                                                       nrevs)))))
    (min nrevs have-revs)))

(defun my-git-get-log-range ()
  "Get something like HEAD~100..HEAD (with the `100' sanitized to
only go back as far as the git history) to pass to magit-log."
  (format "HEAD~%d..HEAD"
          (my-git-nrevs-gate 100)))

(setq magit-log-default-log-range 'my-git-get-log-range)
