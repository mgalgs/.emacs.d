;;; init-writing.el --- Distraction-free writing mode  -*- lexical-binding: t; -*-

(require 'olivetti)
(require 'topspace)

(setq topspace-empty-line-indicator
      (lambda () (propertize "~" 'face 'shadow)))

(define-minor-mode m/writing-mode
  "Distraction-free writing mode (olivetti + topspace)."
  :lighter " Writing"
  (let ((arg (if m/writing-mode 1 -1)))
    (olivetti-mode arg)
    (topspace-mode arg)))

(provide 'init-writing)
;;; init-writing.el ends here
