(require 'smart-mode-line)
(require 'rich-minority)

(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 25)
(setq sml/mode-width 'full)

(setq rm-blacklist '(" AC"
                     " SP"
                     " mate"
                     " Plugged"
                     " Gtags"
                     " Abbrev"
                     " Fill"
                     " Guide"
                     " pair"
                     " yas"
                     " MRev"
                     " FN"
                     " Fly"
                     " MML"))

;;; put sml/replacer-regexp-list items in ~/private.el. Stuff like
;;; this:
;; (eval-after-load 'smart-mode-line
;;   '(progn
;;      (add-to-list 'sml/replacer-regexp-list '("/home/mgalgs/workspace" ":WS:") t)
;;      (add-to-list 'sml/replacer-regexp-list '(":WS:/stuff" ":st:") t)))
