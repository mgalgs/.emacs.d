;; yasnippet mode
(require 'yasnippet)
(yas/initialize)
(setq yas/snippet-dirs '("~/.emacs.d/mysnippets"
                         "~/.emacs.d/site-lisp/yasnippet/snippets"))
(mapc 'yas/load-directory yas/snippet-dirs)

