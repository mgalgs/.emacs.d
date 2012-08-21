(require 'auto-complete-config nil t)

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols) ac-sources))
  (auto-complete-mode))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
