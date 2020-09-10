(require 'org)

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

;; (require 'org-gnus)

;; fontify code in code blocks
(setq org-src-fontify-natively t)

;; don't say `days' in clocksums
(setq org-time-clocksum-format "%d :%02d")

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
