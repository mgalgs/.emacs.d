;;; epy-completion.el --- A few common completion tricks

;; Just python
(add-hook 'python-mode-hook 
	  (lambda () 
	    (define-key python-mode-map "'" 'skeleton-pair-insert-maybe)))

;; Live completion with auto-complete
;; (see http://cx4a.org/software/auto-complete/)
(require 'auto-complete-config nil t)

;; Do What I Mean mode
(setq ac-dwim t)
(ac-config-default)

;; set also the completion for eshell
(add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)

(provide 'epy-completion)
;;; epy-completion.el ends here
