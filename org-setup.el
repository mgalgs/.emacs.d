(require 'org-install)

(setq org-use-property-inheritance t)   ;for inheritance in matches
;; handy sparse tree shortcuts:
(setq org-agenda-custom-commands
      '(("w" "Currently working on" tags-tree "workon")
        ("p" . "Prefix: Priority trees")                      ;prefix description
        ("pa" "A-Priority todos (TODO)" tags-tree "PRIORITY=\"A\"/TODO")
        ("pA" "A-Priority todos" tags-tree "PRIORITY=\"A\"")
        ("pb" "B-Priority todos (TODO)" tags-tree "PRIORITY=\"B\"/TODO")
        ("pB" "B-Priority todos" tags-tree "PRIORITY=\"B\"")
        ("pc" "C-Priority todos (TODO)" tags-tree "PRIORITY=\"C\"/TODO")
        ("pC" "C-Priority todos" tags-tree "PRIORITY=\"C\"")))
;; TODO workflow states
(setq org-todo-keywords
      '((sequence "TODO" "CODE REVIEW" "BLOCKED" "DONE")))
;; faces for todo keywords:
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("CODE REVIEW" . (:foreground "dark orange"))
        ("BLOCKED" . shadow)
        ("CANCELED" . (:foreground "dark red"))
        ("DONE" . (:foreground "dark green" :weight bold))))

;; fix some org-mode + yasnippet conflicts (if we have loaded yas):
(unless (not (boundp 'yas/version))
  (defun yas/org-very-safe-expand ()
    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (make-variable-buffer-local 'yas/trigger-key)
	      (setq yas/trigger-key [tab])
	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	      (define-key yas/keymap [tab] 'yas/next-field))))
