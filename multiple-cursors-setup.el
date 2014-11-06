(require 'multiple-cursors)
(require 'phi-search)

;; credit to @jonebird for the following 
;; Allow isearch functionality with multipl-cursors
(add-hook 'multiple-cursors-mode-enabled-hook
          (lambda ()
            (interactive)
            (global-set-key (kbd "C-s") 'phi-search)
            (global-set-key (kbd "C-r") 'phi-search-backward)))

(add-hook 'multiple-cursors-mode-disabled-hook
          (lambda ()
            (interactive)
            (global-set-key (kbd "C-s") 'isearch-forward)
            (global-set-key (kbd "C-r") 'isearch-backward)))
