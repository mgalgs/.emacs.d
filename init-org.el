;; (require 'org-install)
(require 'org)
;; (require 'htmlize)
;; (setq htmlize-ignore-faces '(whitespace-line whitespace-indentation))

(require 'ox-md)

(setq org-default-notes-file "~/notes/notes.org")
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
        ("pC" "C-Priority todos" tags-tree "PRIORITY=\"C\"")
        ("d" "[d]eadline-less tasks" tags "-DEADLINE={.+}/!+TODO")))
;; TODO workflow states
(setq org-todo-keywords
      '((sequence "TODO" "NEED DV" "CODE REVIEW" "BLOCKED" "DEFERRED" "DONE")))
;; faces for todo keywords:
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("NEED DV" . (:foreground "yellow"))
        ("CODE REVIEW" . (:foreground "dark orange"))
        ("BLOCKED" . shadow)
	("DEFERRED" . shadow)
        ("CANCELED" . (:foreground "dark red"))
        ("DONE" . (:foreground "dark green" :weight bold))))

;; fix some org-mode + yasnippet conflicts (if we have loaded yas):
;; (unless (not (boundp 'yas/version))
;;   (defun yas/org-very-safe-expand ()
;;     (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
;;   (add-hook 'org-mode-hook
;; 	    (lambda ()
;; 	      (make-variable-buffer-local 'yas/trigger-key)
;; 	      (setq yas/trigger-key [tab])
;; 	      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
;; 	      (define-key yas/keymap [tab] 'yas/next-field))))

(require 'org-gnus)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; don't say `days' in clocksums
(setq org-time-clocksum-format "%d :%02d")

; (require 'ox-reveal)

(setq org-capture-templates
      '(("t" "Todo item (including gnus links)" entry
         (file+headline "~/notes/notes.org" "Tasks")
         "* TODO %?\n  Link: %a\n\n  %i" :prepend t)
        ("c" "Todo item (no link)" entry
         (file+headline "~/notes/notes.org" "Tasks")
         "* TODO %?\n  %i" :prepend t)))

;; enable graphviz dot
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)))

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(setq org-confirm-babel-evaluate nil)

;;; from http://emacs.stackexchange.com/q/3374/573
(defun m/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq org-html-head-extra
            (concat org-html-head-extra
                    (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                            my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'm/org-inline-css-hook)
