;; gnu global
(require 'gtags)
;; need this hook to run after my-cedet-hook because global hijacks
;; M-. and M-* which we want to use with cedet.
(add-hook 'c-mode-common-hook
          '(lambda ()
             (gtags-mode 1)))

(defun my-gtags-next-result ()
  "Go to the next result after a gtags-find-rtag etc."
  (interactive)
  (let ((the-gtags-select-buffer (get-buffer-by-regexp "\*GTAGS SELECT\*.*")))
    (if (not the-gtags-select-buffer)
        (message "No GTAGS SELECT buffer found!")
      (with-current-buffer the-gtags-select-buffer
        (setq max-linum (- (line-number-at-pos (point-max)) 1))
        (if (> my-gtags-current-result-line max-linum )
            (progn
              (message "Moved past last result. Go again to start over.")
              (setq my-gtags-current-result-line 1))
          (goto-char (point-min))
          (message (format "Moving to line %d" my-gtags-current-result-line))
          (forward-line (- my-gtags-current-result-line 1))
          (setq my-gtags-current-result-line (1+ my-gtags-current-result-line))
          (gtags-select-tag))))))

(make-variable-buffer-local 'my-gtags-current-result-line)
(set-default 'my-gtags-current-result-line 1)
(global-set-key "\C-cgn" 'my-gtags-next-result)


(if my-switch-with-cedet-p
    (progn
      (defun my-gtags-mode-hook ()
	(define-key gtags-mode-map (kbd "M-.") 'semantic-goto-definition) ;gtags binds to M-. and M-* which we don't want
	(define-key gtags-mode-map (kbd "M-*") 'semantic-pop-tag-mark))
      (add-hook 'gtags-mode-hook 'my-gtags-mode-hook)))

(defun my-gtags-command-finished (process event)
  (message "%s: %s" process (replace-regexp-in-string (regexp-quote "\n")
						      ""
						      event)))

(defun my-gtags-do-gtags-command-at-project-root (args)
  "Run gtags command (with `ARGS' as a command line argument) at
the project root."
  (let ((project-dir (or (locate-dominating-file (file-name-directory (buffer-file-name)) ".git")
			 (locate-dominating-file (file-name-directory (buffer-file-name)) "GTAGS"))))
    (if (null project-dir)
	(message "Couldn't find project dir!")
      (message (format "Running `gtags %s' at %s" args project-dir))
      (let ((update-process (start-process-shell-command "gtags-update"
							 nil
							 (format "cd %s && gtags %s"
								 project-dir
								 args))))
	(set-process-sentinel update-process 'my-gtags-command-finished)))))

(defun my-gtags-update-tags-file ()
  "Update the gtags tags file"
  (interactive)
  (my-gtags-do-gtags-command-at-project-root "-I"))

(defun my-gtags-update-tags-file-incrementally ()
  "Update the gtags tags file"
  (interactive)
  (my-gtags-do-gtags-command-at-project-root "-iI"))

;;; ADVICE BELOW

;; some advice for vertical recentering when visiting tags...

(my-make-recentering-advice gtags-select-it)
(my-make-recentering-advice gtags-goto-tag)
(my-make-recentering-advice gtags-pop-stack)
;; (my-make-recentering-advice gtags-current-token)
