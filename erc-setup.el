(defun my-erc-ansi-colors ()
  "For ansi color escape sequences"
    (ansi-color-apply-on-region (point-min) (point-max)))

(add-hook 'erc-insert-modify-hook 'my-erc-ansi-colors)
(require 'erc-hl-nicks)

(defvar my-erc-notification-overlays nil)
(defun my-erc-toggle-notifications ()
  (interactive)
  (if my-erc-notification-overlays
      (progn
        (ov-reset my-erc-notification-overlays)
        (setq my-erc-notification-overlays nil))
    (setq my-erc-notification-overlays (ov-regexp "^\\*\\*\\*.*\n"))
    (ov-set my-erc-notification-overlays 'invisible t)))
