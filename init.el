(require 'server)
(unless (server-running-p)
  (server-start))

(defun m/l (file)
  "loads a file from the `user-emacs-directory'"
  (load-file (concat user-emacs-directory file)))

(m/l "package-setup.el")

(setq load-path
      (append (list "~/.emacs.d/lisp")
              load-path))

;; workaround for https://lists.gnu.org/archive/html/emacs-devel/2015-07/msg00251.html
;; remove once http://lists.gnu.org/archive/html/emacs-diffs/2015-03/msg00137.html is included in a stable release
(setq tramp-ssh-controlmaster-options nil)

;;; begin some misc setup. This should be first because it's
;;; distracting to switch up UI elements later during loading.
(when window-system
  (tool-bar-mode 0)
  (set-scroll-bar-mode 'right))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin"))

;; some general advice
(m/l "my-advice.el")

;; some utility functions
(m/l "my-util.el")

(when (file-exists-p "~/private.el")
    (load-file "~/private.el"))

;; misc settings
(setq scroll-step 1
      default-truncate-lines t
      display-time-day-and-date t
      nxml-sexp-element-flag t
      linum-format "%4d"
      scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      inhibit-startup-screen t
      isearch-allow-scroll t
      kill-whole-line t
      show-trailing-whitespace t
      ring-bell-function 'ignore
      history-length 6000
      compile-command "make"
      read-file-name-completion-ignore-case t
      mouse-yank-at-point t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.backups-emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq-default indent-tabs-mode nil ; don't use the tab character, only spaces
	      ;; tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
	      ;; tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
	      set-mark-command-repeat-pop t ; repeated C-SPC after C-u C-SPC keeps popping the mark
	      ;; c-default-style "bsd" ; for nasty brace face
	      indicate-buffer-boundaries 'right
	      truncate-lines t
	      fill-column 75
	      ediff-show-clashes-only t
	      term-buffer-maximum-size 15000)

;; auto revert (useful when switching git branches)
(global-auto-revert-mode)

(use-package s)

(use-package dash)

(use-package ggtags)

(use-package whitespace
  :init
  (global-whitespace-mode 0)
  (dolist (hook '(c-mode-common-hook))
    (add-hook hook #'whitespace-mode)))

(use-package yasnippet
  :config
  (yas-reload-all)
  :init
  (dolist (hook '(c-mode-common-hook))
    (add-hook hook #'yas-minor-mode)))

(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  (use-package ox-reveal)
  (use-package htmlize)
  (m/l "org-setup.el"))

(use-package ido
  :init
  (ido-mode)
  (add-hook 'ido-setup-hook
	    (lambda () (define-key ido-completion-map
			 " "
			 'ido-restrict-to-matches)))
  :config
  (setq ido-enable-flex-matching t
	ido-max-prospects 6
	ido-auto-merge-work-directories-length -1
	ido-default-buffer-method 'maybe-frame
	ido-ignore-buffers
	'("\\` " "^\*Back"
	  ".*Completion" "^\*Ido" "^\*trace"
	  "^\*Bookmark" "^\*Compile\-Log"
	  "^\*Buffer List"
	  "^\*Shell Command Output" ;"^\*compilation\*"
	  "^\*RE\-Builder\*"
	  "^\*Pymacs\*" "*Async Shell Command*"
	  "^\.newsrc-dribble"
	  "^\*GTAGS SELECT\*")))

(use-package helm
  :init
  (require 'helm)
  (require 'helm-config)
  (use-package helm-ls-git)
  (use-package helm-swoop)
  (require 'helm-bookmark)
  (setq helm-minibuffer-history-key nil
	helm-truncate-lines t)
  ;; todo: make cycling global (not per-source):
  ;; https://github.com/emacs-helm/helm/issues/387
  (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ls-git-show-abs-or-relative 'relative))

(use-package shell
  :config
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(use-package cc-mode
  :config
  (m/l "linux-kernel-setup.el"))

(use-package iedit
  :bind
  ("C-c m ;" . iedit-mode)
  :config
  (setq iedit-auto-recenter nil))

(use-package pkgbuild-mode)

(use-package no-word
  :ensure nil
  :load-path "lisp/")

(use-package lua-mode)

(use-package csharp-mode)

(use-package browse-kill-ring
  :bind
  ("C-c k" . browse-kill-ring))

(use-package paredit
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook
                  ielm-mode-hook
                  lisp-mode-hook
                  lisp-interaction-mode-hook
                  scheme-mode-hook))
    (add-hook hook #'enable-paredit-mode)))

(use-package magit
  :bind
  (("C-c m t" . magit-status)
   ("C-c m c" . magit-show-commit)
   ("C-c m m c" . m/show-commit-at-point)
   ("C-c m :" . magit-git-command)
   ("C-c m m b" . magit-blame)
   ("C-c m m l" . m/magit-file-log))
  :init
  (use-package git-commit
    :init
    (bind-key "C-c C-e" 'm/suggest-commit-message-prefix))
  :config
  (m/l "magit-setup.el"))

(use-package ansi-color
  :init
  ;; handle ANSI color escape sequences in compilation output (like for
  ;; Android builds) Credit: http://stackoverflow.com/a/20788581/209050
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(use-package gthings
  :ensure nil
  :load-path "lisp/")

(use-package kconfig
  :ensure nil
  :load-path "lisp/")

(use-package which-func
  :config
  (setq which-func-unknown "-")
  (set-face-attribute 'which-func nil
                      :foreground "deep sky blue")
  (setq mode-line-misc-info
        ;; We remove Which Function Mode from the mode line, because it's mostly
        ;; invisible here anyway.
        (assq-delete-all 'which-func-mode mode-line-misc-info))
  (setq which-func-non-auto-modes '(gnus-group-mode
                                    gnus-summary-mode
                                    gnus-article-mode
                                    text-mode
                                    fundamental-mode
                                    help-mode
                                    git-commit-mode
                                    magit-mode)))

(use-package which-c-preprocessor-cond
  :load-path "lisp/"
  :ensure nil
  :config
  (which-c-preprocessor-cond-mode)
  (setq-default header-line-format
                `((which-func-mode ("" which-func-format " "))
                  (which-c-preprocessor-cond-mode ,which-c-preprocessor-cond-format))))

(use-package kernel-stack-trace-mode
  :load-path "lisp/kernel-stack-trace-mode"
  :ensure nil)

(use-package diffview
  :load-path "lisp/diffview-mode"
  :ensure nil)
