;; Doesn't work yet!

(defun my-ov-buffer-background (regexp color)
  (ov-set (ov-regexp regexp)
          (point-min)
          (point-max)
          'face `(:foreground ,color)))

(defun my-overlays-for-mailing-lists ()
  (interactive)
  (let ((regspecs '(("\\[RFC.*\\]" . "chartreuse1")
                    ("\\[PATCH.*\\]" . "dark turquoise")
                    ("\\b[^[:blank:]]+: " . "tomato1")
                    ("\\[GIT PULL\\]" . "red4"))))
    (dolist (r regspecs)
      (my-ov-buffer-background (car r) (cdr r)))))

(add-hook 'gnus-summary-mode-hook 'my-overlays-for-mailing-lists)
