(require 'flyspell)
(add-hook 'git-commit-mode-hook 'flyspell-mode)
(add-hook 'message-mode-hook 'flyspell-mode)
;; unbind C-; since we use it for iedit
(define-key flyspell-mode-map (kbd "C-;") nil)
