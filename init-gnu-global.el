(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(defun m/gtags-command-finished (process event)
  (message "%s: %s" process (replace-regexp-in-string (regexp-quote "\n")
						      ""
						      event)))

(defun m/gtags-do-gtags-command-at-project-root (args)
  "Run gtags command (with `ARGS' as a command line argument) at
the project root."
  (let ((project-dir (or (locate-dominating-file default-directory ".git")
			 (locate-dominating-file default-directory "GTAGS"))))
    (if (null project-dir)
	(message "Couldn't find project dir!")
      (message (format "Running `gtags %s' at %s" args project-dir))
      (let ((update-process (start-process-shell-command "gtags-update"
							 nil
							 (format "cd %s && gtags %s"
								 project-dir
								 args))))
	(set-process-sentinel update-process 'm/gtags-command-finished)))))

(defun m/gtags-update-tags-file ()
  "Update the gtags tags file"
  (interactive)
  (m/gtags-do-gtags-command-at-project-root "-I"))

(defun m/gtags-update-tags-file-incrementally ()
  "Update the gtags tags file"
  (interactive)
  (m/gtags-do-gtags-command-at-project-root "-iI"))

(defun m/recenter-ignoring-args (&rest args)
  (recenter))

(setq ggtags-find-tag-hook 'm/recenter-ignoring-args)
(advice-add 'pop-tag-mark  :after #'m/recenter-ignoring-args)
