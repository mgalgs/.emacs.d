(server-start)			    ;emacs server

(setq load-path
      (append (list nil
                    "~/.emacs.d/site-lisp"
		    "~/.emacs.d/site-lisp/auto-complete"
		    "~/.emacs.d/site-lisp/yasnippet"
		    "~/.emacs.d/python-config")
              load-path))

;; set up python
(load-file "~/.emacs.d/python-config/epy-mitch.el")

(require 'ido)			    ;ido-mode:
(ido-mode)			    ;sweet!
(setq default-truncate-lines t)	    ;no wrapping
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq scroll-step 1)			;scroll step at top/bottom of page
(setq ido-ignore-buffers ;; ignore these guys
      '("\\` " "^\*Mess" "^\*Back"
        ".*Completion" "^\*Ido" "^\*trace"
        "^\*Bookmark" "^\*Compile\-Log"
        "^\*Buffer List" "^\*scratch\*"
        "^\*Shell Command Output" ;"^\*compilation\*"
        "^\*Help\*" "^\*RE\-Builder\*"
		"^\*Pymacs\*" "*Async Shell Command*"
        "^\*GTAGS SELECT\*"))

(setq inhibit-startup-screen t)
(tool-bar-mode 0)
(show-paren-mode t)

;; Line numbering
(setq linum-format "%4d")
(global-linum-mode 1)

(load-file "~/.emacs.d/my-util.el")
(load-file "~/.emacs.d/my-keybindings.el")

(require 'iedit)

(setq nxml-sexp-element-flag t)

;; mozrepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'espresso-mode-hook 'espresso-custom-setup)
(defun espresso-custom-setup ()
  (moz-minor-mode 1))

(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)

;; pkgbuild mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode))
			      auto-mode-alist))

;; yasnippet!
(require 'yasnippet)
(yas/initialize)
(setq yas/snippet-dirs '("~/.emacs.d/mysnippets"
                         "~/.emacs.d/site-lisp/yasnippet/snippets"))
(mapc 'yas/load-directory yas/snippet-dirs)
