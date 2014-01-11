(require 'web-mode)

(defun my-current-buffer-django-p ()
  (save-excursion
    (search-forward-regexp "{% base\\|{% if\\|{% include\\|{% block"
                           nil
                           t)))

(setq web-mode-engines-alist
      '(("django". "\\.djhtml")
        ("django" . my-current-buffer-django-p)))

(define-key web-mode-map (kbd "C-;") nil)
