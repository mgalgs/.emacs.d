(server-start)

;;; begin some misc setup. This should be first because it's
;;; distracting to switch up UI elements later during loading.
(if window-system
    (progn
      (tool-bar-mode 0)
      (set-scroll-bar-mode 'right)))

;; load some functions that are helpful for initialization
(load-file "~/.emacs.d/util-for-init.el")

(setq load-path
      (append
       ;; everyone under site-lisp:
       (directories-in-directory "~/.emacs.d/site-lisp")
       (list nil
	     "~/.emacs.d/auto-install"
	     "~/.emacs.d/python-config"
         ;; the site-lisp directory itself has some packages
         "~/.emacs.d/site-lisp"
	     ;; this guy is under site-lisp but is special because
	     ;; he's a subdirectory:
	     "~/.emacs.d/site-lisp/org-7.7/lisp")
       load-path))

;; some utility functions
(load-file "~/.emacs.d/my-util.el")

(if (file-exists-p "~/private.el")
    (load-file "~/private.el"))

;; set up python
(load-file "~/.emacs.d/python-config/epy-mitch.el")

;; set up cedet
(load-file "~/.emacs.d/cedet-setup.el")

;; set up gnu global
(load-file "~/.emacs.d/gnu-global-setup.el")

;; set up yasnippet
(load-file "~/.emacs.d/yasnippet-setup.el")

;; set up org mode
(load-file "~/.emacs.d/org-setup.el")

;; set up a bunch of auto-mode-alist stuff
(load-file "~/.emacs.d/auto-mode-alist-setup.el")

;; key chord
(require 'key-chord)
(key-chord-mode 1)

;; set up ido mode
(load-file "~/.emacs.d/ido-setup.el")

(show-paren-mode t)

; display date and time in status bar
(display-time)

;; ansi color escape codes fun:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(setq
 skeleton-pair t
 display-time-day-and-date t
 nxml-sexp-element-flag t
 linum-format "%4d"
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1
 inhibit-startup-screen t
 )

(setq-default
 indent-tabs-mode nil ; don't use the tab character, only spaces
 ;indent-tabs-mode t ; use the tab character, not spaces
 c-basic-offset 4 ; how many spaces our tab key will insert
 c-default-style "bsd" ; for nasty brace face
 tab-width 8 ; default tab-width (when there are existing tabs in files)
 )

;;; end misc setup

;; force some c++ keybindings
(defun my-c++-mode-keybindings-hook ()
  (local-set-key "\C-\M-e" 'end-of-defun)
  (local-set-key "\C-\M-a" 'beginning-of-defun))
(add-hook 'c++-mode-hook 'my-c++-mode-keybindings-hook)

(require 'iedit)

;; mozrepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'espresso-mode-hook 'espresso-custom-setup)
(defun espresso-custom-setup ()
  (moz-minor-mode 1))

;; Line numbering
;; (global-linum-mode 1)

;; espresso mode
(autoload 'espresso-mode "espresso" nil t)

;; pkgbuild mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

;; mode to read word documents
(require 'no-word)

;; lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; rfcview mode
;; (autoload 'rfcview-mode "rfcview" nil t)
(require 'rfcview)

;; rfc downloader:
(autoload 'get-rfc-view-rfc "get-rfc" "Get and view an RFC" t nil)
(autoload 'get-rfc-view-rfc-at-point "get-rfc" "View the RFC at point" t nil)
(autoload 'get-rfc-grep-rfc-index "get-rfc" "Grep rfc-index.txt" t nil)
(setq get-rfc-rfcs-local-flag t)
(setq get-rfc-open-in-new-frame nil)

;; bc mode
(autoload 'bc-mode "bc-mode.el" "bc-mode" t 'nil)
(add-to-list 'interpreter-mode-alist '("bc" . bc-mode))

;; c-sharp mode:
(autoload 'csharp-mode "csharp-mode" nil t)

;; kill ring usefulness:
(autoload 'browse-kill-ring "browse-kill-ring" nil t)

;; autopair
;; (require 'autopair)
;; (autopair-global-mode) ;; to enable in all buffers

;; gpg use the minibuffer for passphrase, not GUI
(setenv "GPG_AGENT_INFO" nil)

;; show column number in status bar:
(column-number-mode)

;; pkgbuild mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

;; indent-hints minor mode
(require 'indent-hints)
(indent-hints-global-mode)

;; better buffer disambiguation
(require 'uniquify)

;; ace mode jumping
(require 'ace-jump-mode)
(add-hook 'ace-jump-mode-before-jump-hook 'push-mark)

;; batch files
(autoload 'bat-mode "bat-mode" "DOS and WIndows BAT files" t)

;; textmate clonage
(require 'textmate)
(textmate-mode)
(add-hook 'textmate-goto-symbol-hook 'push-mark)

;; zenburn theme
(if window-system
    (progn
      (require 'color-theme-zenburn)
      (color-theme-zenburn)))

;; gist fun
(require 'gist)

;; iy go to char motion helper
(require 'iy-go-to-char)

;; auto install
(require 'auto-install)

;; one key
(require 'one-key)
(load-file "~/.emacs.d/one-key-setup.el")

;; lazy search
(require 'lazy-search)
(setq-default lazy-search-keep-region nil)

;; tbemail for thunderbird composition
(require 'tbemail)

;; markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; yaml mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing yaml files" t)

;; transpose split windows
(load-file "~/.emacs.d/auto-install/transpose.el")

;; column marker
(require 'column-marker)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-3 80)))

;; browse kill ring visually
(require 'browse-kill-ring)

;; pretty control L page breaks
(require 'pp-c-l)
(pretty-control-l-mode 1)

;; some keybindings
(load-file "~/.emacs.d/my-keybindings.el")

;; some general advice
(load-file "~/.emacs.d/my-advice.el")

;; cool-looking mode-line
(require 'powerline)
;; fix up the powerline modeline colors
(set-face-attribute 'mode-line nil
                    :background "turquoise4"
                    :foreground "white"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :background "grey15"
                    :foreground "grey78"
                    :box nil)
