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


(unless my-switch-no-cedet-p
  (defun my-gtags-mode-hook ()
    (define-key gtags-mode-map (kbd "M-.") 'semantic-goto-definition) ;gtags binds to M-. and M-* which we don't want
    (define-key gtags-mode-map (kbd "M-*") 'semantic-pop-tag-mark))

  (add-hook 'gtags-mode-hook 'my-gtags-mode-hook))

(defun my-gtags-regenerate-tags-file-async ()
  "Regenerate the tags files"
  (interactive)
  (unless gtags-rootdir (gtags-visit-rootdir))
  (message "Starting gtags in the background...")
  (start-process-shell-command "gtags"
			       (concat "*GTAGS REGENERATE* " gtags-rootdir)
			       (concat "cd "
				       gtags-rootdir
				       " && gtags -I")))

(defun my-gtags-regenerate-tags-file ()
  "Regenerate the tags files"
  (interactive)
  (unless gtags-rootdir (gtags-visit-rootdir))
  (shell-command (concat "cd " gtags-rootdir " && gtags -I")))

(defun my-gtags-update-tags-file-incrementally ()
  "Update the tags files incrementally"
  (interactive)
  (unless gtags-rootdir (gtags-visit-rootdir))
  (shell-command (concat "cd " gtags-rootdir " && gtags -I -i")))

;;; ADVICE BELOW

;; some advice for vertical recentering when visiting tags...

(my-make-recentering-advice gtags-select-it)
(my-make-recentering-advice gtags-goto-tag)
(my-make-recentering-advice gtags-pop-stack)
;; (my-make-recentering-advice gtags-current-token)
