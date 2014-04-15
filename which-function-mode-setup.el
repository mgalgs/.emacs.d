(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq which-func-unknown "-")
(set-face-attribute 'which-func nil
                    :foreground "deep sky blue")
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))
(setq which-func-non-auto-modes '(gnus-group-mode
                                  gnus-summary-mode
                                  gnus-article-mode
                                  text-mode
                                  fundamental-mode
                                  help-mode
                                  git-commit-mode
                                  magit-mode))
