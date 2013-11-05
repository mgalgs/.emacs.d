(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))

(setq sml/shorten-directory t)
(setq sml/shorten-modes t)
(setq sml/name-width 30)
(setq sml/mode-width 'full)

(add-to-list 'sml/hidden-modes " AC")
(add-to-list 'sml/hidden-modes " SP")
(add-to-list 'sml/hidden-modes " mate")
(add-to-list 'sml/hidden-modes " Plugged")
(add-to-list 'sml/hidden-modes " Gtags")
(add-to-list 'sml/hidden-modes " Abbrev")
(add-to-list 'sml/hidden-modes " Fill")

;;; put sml/replacer-regexp-list items in ~/private.el. Stuff like
;;; this:
;;     (add-to-list 'sml/replacer-regexp-list '("/home/mgalgs/workspace" ":WS:") t)
;;     (add-to-list 'sml/replacer-regexp-list '(":WS:/stuff" ":st:") t)
