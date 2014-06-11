(defun my-erc-ansi-colors ()
  "For ansi color escape sequences"
    (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'erc-insert-modify-hook 'my-erc-ansi-colors)
