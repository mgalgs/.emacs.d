(provide 'epy-mitch)

(require 'epy-python)
(require 'epy-completion)

;;; hoisted from epy-editing.el ;;;
;; Auto Completion
;; (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))

;;; hoisted from epy-bindings.el ;;;
;; Rope bindings
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map "\C-ci" 'rope-auto-import)
	    (define-key python-mode-map "\C-c\C-d" 'rope-show-calltip)))

;;; some more bindings ;;;
(global-set-key "\C-c\t" 'yas/next-field)
