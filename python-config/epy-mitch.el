(provide 'epy-mitch)

(require 'epy-python)
(require 'epy-completion)

;;; hoisted from epy-editing.el ;;;
;; Auto Completion
;; (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
;;; hoisted from epy-bindings.el ;;;

;;; some more bindings ;;;
(global-set-key "\C-c\t" 'yas/next-field)
