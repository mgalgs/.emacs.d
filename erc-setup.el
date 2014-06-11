(defun my-erc-ansi-colors ()
  "For ansi color escape sequences"
  (when (region-active-p)
    (ansi-color-apply-on-region (region-beginning) (region-end))))

(add-hook 'erc-insert-modify-hook 'my-erc-ansi-colors)
