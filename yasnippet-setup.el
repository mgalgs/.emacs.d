(require 'yasnippet)
(setq yas-dont-activate
      (lambda ()
        (eq major-mode 'term-mode)))
(yas-global-mode 1)
