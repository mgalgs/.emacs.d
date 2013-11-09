(require 'diff-hl)
(diff-hl-mode)
(add-hook 'magit-refresh-file-buffer-hook 'diff-hl-update)
