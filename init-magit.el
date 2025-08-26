(setq magit-last-seen-setup-instructions "1.4.0")

;; avoid conflicts with vc:
;; (remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq magit-diff-refine-hunk 'all)
(setq git-rebase-auto-advance t)
(setq magit-stage-all-confirm nil)
(setq magit-commit-squash-commit 'marked-or-current)
(setq magit-push-always-verify nil)     ; cuz it says so

(defun m/magit-display-buffer-traditional (buffer)
  "Like magit-display-buffer-traditional, but re-uses window for status mode, too."
  (display-buffer
   buffer (if (not (memq (with-current-buffer buffer major-mode)
                         '(magit-process-mode
                           magit-revision-mode
                           magit-diff-mode
                           magit-stash-mode)))
              '(display-buffer-same-window)
            nil)))

(setq magit-display-buffer-function 'm/magit-display-buffer-traditional)

(setq magit-log-section-commit-count' 10)
(setq magit-log-section-arguments '("--graph" "--decorate" "--color"))

(setq magit-status-sections-hook
      '(magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        magit-insert-unpushed-cherries
        magit-insert-untracked-files
        magit-insert-recent-commits
        magit-insert-unpulled-from-pushremote
        magit-insert-unpushed-to-upstream
        magit-insert-unpushed-to-pushremote
        magit-insert-unpulled-from-upstream
        magit-insert-stashes))

(font-lock-add-keywords 'emacs-lisp-mode
                        magit-font-lock-keywords)

(defun m/magit-reset-author (&optional args)
  "Resets the authorship information for the last commit"
  (interactive)
  (magit-run-git-async "commit" "--amend" "--no-edit" "--reset-author"))

;; (magit-define-popup-action 'magit-commit-popup
;;   ?R "Reset author" 'm/magit-reset-author)
(transient-append-suffix 'magit-commit
  "S"
  '("R" "Reset author" m/magit-reset-author))

;; Pretty nice-looking faces for dark theme
(custom-set-faces
 ;; Added lines – deeper green bg, brighter fg
 '(magit-diff-added
   ((t (:background "#003300" :foreground "#61ef61"))))
 '(magit-diff-added-highlight
   ((t (:background "#145214" :foreground "#90ff90" :weight bold))))
 '(diff-refined-added
   ((t (:background "#227722" :foreground "#c0ffc0" :weight bold))))

 ;; Removed lines – deeper red bg, brighter fg
 '(magit-diff-removed
   ((t (:background "#330000" :foreground "#ff6666"))))
 '(magit-diff-removed-highlight
   ((t (:background "#662222" :foreground "#ff9494" :weight bold))))
 '(diff-refined-removed
   ((t (:background "#883333" :foreground "#ffd0d0" :weight bold))))

 ;; Context lines – slightly brighter than before
 '(magit-diff-context
   ((t (:background "#202020" :foreground "#bbbbbb"))))
 '(magit-diff-context-highlight
   ((t (:background "#303030" :foreground "#dddddd"))))

 '(magit-diff-hunk-heading ((t (:background "#303030" :foreground "lightgray" :weight bold))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#505050" :foreground "white" :weight bold))))
 '(magit-diff-hunk-heading-selection ((t (:background "#606090" :foreground "white" :weight bold))))

 '(magit-diff-file-heading ((t (:foreground "LightSteelBlue" :weight bold))))
 '(magit-diff-file-heading-highlight ((t (:background "#333344" :weight bold))))
 '(magit-diff-file-heading-selection ((t (:background "#666699" :foreground "white" :weight bold)))))
