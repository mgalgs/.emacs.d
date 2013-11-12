(require 'diff-hl)
(global-diff-hl-mode)
(add-hook 'magit-refresh-file-buffer-hook 'diff-hl-update)
