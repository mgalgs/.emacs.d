(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
;; unbind C-; since we use it for iedit
(define-key flyspell-mode-map (kbd "C-;") nil)
