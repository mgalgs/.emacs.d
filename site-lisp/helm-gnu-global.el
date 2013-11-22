(defvar helm-gnu-global-global-command nil
  "Command name of global.")

;; Set global's command name
(setq helm-gnu-global-global-command
      (getenv "GTAGSGLOBAL"))
(if (or (not helm-gnu-global-global-command)
        (equal helm-gnu-global-global-command ""))
    (setq helm-gnu-global-global-command
          "global"))

(defun helm-gnu-global-get-symbols ()
  (with-temp-buffer
    (call-process helm-gnu-global-global-command nil t nil "-c")
    (split-string (buffer-string))))

(defun helm-gnu-global-visit (el)
  (gtags-push-context)
  (gtags-goto-tag el "C"))

(setq helm-source-gnu-global-symbols
      '((name . "Gnu Global Symbols")
        (candidates . helm-gnu-global-get-symbols)
        (action . (("Visit" . helm-gnu-global-visit)))
        (requires-pattern . 2)
        (delayed . 1)))

(defun helm-gnu-global ()
  (interactive)
  (helm :sources '(helm-source-gnu-global-symbols)
        :buffer "*helm gnu global*"))

(provide 'helm-gnu-global)
