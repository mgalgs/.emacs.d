(require 'smartparens)
(setq sp-navigate-reindent-after-up nil)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;; pair "`" with "'" in emacs-lisp-mode
(sp-local-pair 'emacs-lisp-mode "`" "'")
;; no '' pair in emacs-lisp-mode
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

;; don't pair "'" if we're at the end of a word (like when typing an
;; apostrophe)
(sp-pair "'" nil :unless '(sp-point-after-word-p))
