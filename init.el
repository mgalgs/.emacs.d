(require 'server)
(unless (server-running-p)
  (server-start))

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; (especially useful for speeding up lsp-mode)
(setq gc-cons-threshold 50000000)

(defun m/l (file)
  "loads a file from the `user-emacs-directory'"
  (load-file (concat user-emacs-directory file)))

(m/l "init-package.el")

(defmacro use-package-nope (&rest _)
  "Ignore the entire use-package block. Intended to temporarily disable a
use-package declaration without commenting."
  nil)

(setq load-path
      (append (list "~/.emacs.d/lisp")
              load-path))

(m/l "init-straight.el")

;; workaround for https://lists.gnu.org/archive/html/emacs-devel/2015-07/msg00251.html
;; remove once http://lists.gnu.org/archive/html/emacs-diffs/2015-03/msg00137.html is included in a stable release
(setq tramp-ssh-controlmaster-options nil)

;;; begin some misc setup. This should be first because it's
;;; distracting to switch up UI elements later during loading.
(when window-system
  (tool-bar-mode 0)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

;;; mac-specific setup
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'control))

;;; chrome os (crostini) specific setup
(when (getenv "SOMMELIER_VERSION")
  (define-key key-translation-map (kbd "<delete>") (kbd "<M-DEL>")))

;; some general advice
(m/l "my-advice.el")

;; some utility functions
(use-package my-util
  :ensure nil
  :commands (m/suggest-commit-message-prefix)
  :load-path "lisp/"
  :bind
  (("C-c m m a" . m/add-include)
   ("<M-up>" . m/increment-number-decimal)
   ("<M-down>" . m/decrement-number-decimal)
   ("M-P" . m/up-a-line)
   ("M-N" . m/down-a-line)
   ("C-S-l" . m/horizontal-recenter)
   ("C-c m d" . m/lookup-current-word)
   ("<f8>" . m/toggle-tab-width-setting)
   ("C-c m i" . m/kill-where-i-am)
   ("C-c m m i" . m/kill-where-i-am-relative-to-gitroot)
   ("C-c m g" . m/grep-what-im-on)
   ("C-c m l" . m/kill-last-message)
   ("C-c m x" . m/xdg-open-each-in-region)
   ("C-c m m x" . m/shell-here)
   ("C-c i" . m/go-to-corresponding-header-or-implementation-file)
   ("C-c m `" . m/recompile)
   ("C-c m m `" . m/open-compilation-buffer)
   ("C-c m m o" . m/occur-region-or-symbol-at-point)
   ("C-c m C-x C-e" . eval-and-replace)
   ("C-c C-h e" . m/view-and-switch-to-echo-area-messages)
   ("C-c m M-/" . m/expand-file-name-at-point)
   ("C-c m m u" . m/underline-previous-line)
   ("C-c m C-y" . m/get-primary)
   ("M-Y" . m/yank-pop-forwards)
   ("C-c m M-%" . m/query-replace-using-region)
   ("C-M-." . m/xref-find-apropos-at-point)
   (:map
    git-commit-mode-map
    ("C-c C-e" . m/suggest-commit-message-prefix))))

(defvar m/init-complete-hook nil
  "Runs when ~/.emacs.d/init.el finishes loading.  Useful for
local overrides.  For example, anything in ~/private.el should
probably run in this hook rather than running at load time, since
it might contain configuration that relies on packages yet to be
installed/loaded.")

(when (file-exists-p "~/private.el")
  (load-file "~/private.el"))


;;; misc settings
(setq scroll-step 1
      default-truncate-lines t
      display-time-day-and-date t
      nxml-sexp-element-flag t
      linum-format "%4d"
      scroll-margin 2
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      inhibit-startup-screen t
      isearch-allow-scroll t
      kill-whole-line t
      ring-bell-function 'ignore
      history-length 6000
      compile-command "make"
      compilation-skip-threshold 2
      read-file-name-completion-ignore-case t
      mouse-yank-at-point t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.backups-emacs-saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      custom-safe-themes t
      dired-listing-switches "-alh"
      uniquify-buffer-name-style 'post-forward-angle-brackets
      column-number-mode t
      auto-revert-verbose nil
      mouse-wheel-mode nil
      confirm-kill-emacs 'y-or-n-p
      visual-line-fringe-indicators '(right-arrow nil)
      require-final-newline t)

(setq-default indent-tabs-mode nil ; don't use the tab character, only spaces
              ;; tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
              ;; tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
              set-mark-command-repeat-pop t ; repeated C-SPC after C-u C-SPC keeps popping the mark
              ;; c-default-style "bsd" ; for nasty brace face
              indicate-buffer-boundaries 'right
              truncate-lines t
              fill-column 85
              ediff-show-clashes-only t
              term-buffer-maximum-size 15000)

(fset 'yes-or-no-p 'y-or-n-p)
(put 'narrow-to-region 'disabled nil)

;; gpg use the minibuffer for passphrase, not GUI
(setenv "GPG_AGENT_INFO" nil)


;;; misc modes

;; auto revert (useful when switching git branches)
(global-auto-revert-mode)
(delete-selection-mode)
(savehist-mode 1)
(electric-pair-mode)
(display-time)
(show-paren-mode)

(defmacro m/visit-init-file-maker ()
  "Defines a function to visit init.el"
  (let ((init-file-name load-file-name))
    `(defun m/visit-init-file ()
       "Visits init.el"
       (interactive)
       (find-file ,init-file-name))))

;; define m/visit-init-file by calling
;; m/visit-init-file-maker
(m/visit-init-file-maker)

;; (load-theme 'wombat)
;;; wombat sets a foreground color for the `region' face, so we need to
;;; make it unspecified so that foreground colors don't get wiped out
;; (set-face-attribute 'region nil :background "MediumPurple4" :foreground 'unspecified)
;; (set-cursor-color "#ff8844")

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-ocean t))
(blink-cursor-mode 0)


;;; *** PACKAGES ***

(use-package s)

(use-package dash)

(use-package diminish)

(use-package auto-complete
  :config
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-mode-map (kbd "C-c /") 'auto-complete)
  (define-key ac-completing-map (kbd "C-s") 'ac-isearch))

(use-package ggtags
  :bind
  ("M-*" . pop-tag-mark)
  ("C-c m M-." . ggtags-find-tag-continue)
  :init
  (dolist (hook '(c-mode-common-hook
                  dired-mode-hook
                  dts-mode-hook
                  conf-mode-hook
                  asm-mode-hook
                  kconfig-mode-hook
                  makefile-mode-hook))
    (add-hook hook (lambda ()
                     (when (locate-dominating-file "." "GTAGS")
                       (ggtags-mode 1)))))
  (m/l "init-gnu-global.el")
  :config
  (setq ggtags-global-window-height 30))

(use-package whitespace
  :init
  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'python-mode-hook (lambda ()
                                (set (make-local-variable 'whitespace-style)
                                     '(face trailing lines-tail empty indentation::space))
                                (setq-local whitespace-line-column 100)
                                (whitespace-mode)))
  :config
  (setq whitespace-style '(face trailing lines-tail empty indentation::tab))
  (global-whitespace-mode 0))

(use-package yasnippet
  :init
  (add-hook 'c-mode-common-hook 'yas-minor-mode)
  (add-hook 'js-mode-hook 'yas-minor-mode)
  (add-hook 'web-mode-hook 'yas-minor-mode)
  :config
  (yas-reload-all))

(use-package org
  :init (m/l "init-org.el")
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda))
  :config
  ;; (use-package ox-reveal)
  (use-package htmlize)
  ; Disable M-h since our UHK requires its use
  (define-key org-mode-map (kbd "M-h") nil))

(use-package-nope ido
  :init
  (ido-mode 1)
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
	  "^\*Pymacs\*"
          "^\.newsrc-dribble"
	  "^\*GTAGS SELECT\*"
          "^\*ag search "))

  (add-to-list 'ido-ignore-files "__pycache__/" t)

  (use-package ido-vertical-mode
    :init
    (ido-vertical-mode 1)
    :config
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(use-package helm
  :bind
  (;; ("C-x C-b" . helm-buffers-list)
   ;; ("C-x r h" . helm-bookmarks)
   :map helm-map
   ("C-s" . helm-next-line)
   ("C-r" . helm-previous-line))
  :init
  (require 'helm)
  (require 'helm-bookmark)
  (setq helm-minibuffer-history-key nil
	helm-truncate-lines t)
  ;; todo: make cycling global (not per-source):
  ;; https://github.com/emacs-helm/helm/issues/387
  ;; (setq helm-move-to-line-cycle-in-source t)
  (setq helm-ls-git-show-abs-or-relative 'relative))

(use-package shell
  :init
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

(use-package cc-mode
  :config
  (m/l "init-linux-kernel.el"))

(use-package iedit
  :bind
  (("C-;" . iedit-mode)
   ("C-c m ;" . iedit-mode))
  :config
  (setq iedit-auto-recenter nil)
  (define-key iedit-lib-keymap (kbd "C-c m '") 'iedit-toggle-unmatched-lines-visible))

(use-package pkgbuild-mode)

(use-package no-word
  :ensure nil
  :load-path "lisp/"
  :mode ("\\.doc\\'" . no-word))

(use-package lua-mode)

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
                  scheme-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook #'enable-paredit-mode)
    (add-hook hook (lambda () (electric-pair-local-mode 0))))
  :diminish paredit-mode
  :bind
  (:map paredit-mode-map
        ("<return>" . my/paredit-RET))
  :config
  ;; from https://www.reddit.com/r/emacs/comments/101uwgd/comment/jjq0jen/
  (defun my/paredit-RET ()
    "Wraps `paredit-RET' to provide a sensible minibuffer experience"
    (interactive)
    (cond
     ((minibufferp)
      (read--expression-try-read))
     ((and (eq major-mode 'inferior-emacs-lisp-mode)
           (string-prefix-p "*ielm*" (buffer-name)))
      (ielm-return))
     (t
      (paredit-RET)))))

(use-package magit
  :bind
  (("C-c m t" . magit-status)
   ("C-c m T" . magit-commit)
   ("C-c m c" . magit-show-commit)
   ("C-c m m c" . m/show-commit-at-point)
   ("C-c m :" . magit-git-command)
   ("C-c m m b" . magit-blame)
   ("C-c m m l" . magit-log-buffer-file))
  :config
  (m/l "init-magit.el"))

(use-package git-commit
  :after magit
  :ensure nil  ; it's part of magit
  :config
  (add-to-list 'git-commit-trailers "Change-Id")
  (add-to-list 'git-commit-trailers "CRs-Fixed")
  (add-to-list 'git-commit-trailers "Git-commit")
  (add-to-list 'git-commit-trailers "Git-repo")
  (add-to-list 'git-commit-trailers "Fixes")
  (add-to-list 'git-commit-trailers "Commit-Message-Co-Author")
  (setq git-commit-finish-query-functions nil)

  ;; Set fill-column only in git-commit-mode
  (add-hook 'git-commit-mode-hook
            (lambda ()
              (setq-local fill-column 72))))

(use-package ansi-color
  :init
  ;; handle ANSI color escape sequences in compilation output (like for
  ;; Android builds) Credit: http://stackoverflow.com/a/20788581/209050
  (defun m/colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'm/colorize-compilation-buffer))

(use-package gthings
  :ensure nil
  :load-path "lisp/")

(use-package kconfig
  :ensure nil
  :mode (("/Kconfig$" . kconfig-mode)
         ("/Kconfig\\..*$" . kconfig-mode))
  :load-path "lisp/")

(use-package which-func
  :config
  (which-function-mode)
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

(use-package kernel-stack-trace-mode
  :load-path "lisp/kernel-stack-trace-mode"
  :ensure nil)

(use-package diffview
  :bind
  ("C-c m m >" . diffview-current)
  :load-path "lisp/diffview-mode"
  :ensure nil)

(use-package diff-mode
  :config
  (define-key diff-mode-map "q" 'bury-buffer))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)
   ("C-c m =" . er/expand-region)))

(use-package google-this
  :bind-keymap ("C-c m /" . google-this-mode-submap)
  :diminish google-this-mode)

(use-package highlight-symbol
  :bind
  (("C-c M-s h ." . highlight-symbol-at-point)
   ("C-c M-s h n" . highlight-symbol-next)
   ("C-c M-s h p" . highlight-symbol-prev)
   ("C-c M-s h a" . highlight-symbol-remove-all)))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C-c m C" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-c m >" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c m <" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-c m m <" . mc/mark-all-like-this))
  :init
  (use-package phi-search
    :config
    (setq phi-search-limit 5000)
    :init
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
                (global-set-key (kbd "C-r") 'isearch-backward)))))

(use-package wgrep)
(use-package wgrep-ag)

;;; make sure you `pacman -S python2-jedi python-jedi'
;; (use-package jedi
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup))

(use-package litable
  :defer t)

(use-package smart-mode-line
  :config
  (setq sml/theme 'dark)
  (sml/setup)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/name-width 25)
  (setq sml/mode-width 'full)
  ;;; put sml/replacer-regexp-list items in ~/private.el. Stuff like
  ;;; this:
  ;; (eval-after-load 'smart-mode-line
  ;;   '(progn
  ;;      (add-to-list 'sml/replacer-regexp-list '("/home/mgalgs/workspace" ":WS:") t)
  ;;      (add-to-list 'sml/replacer-regexp-list '(":WS:/stuff" ":st:") t)))
  (use-package rich-minority
    :config
    (setq rm-blacklist '(" AC"
                         " SP"
                         " mate"
                         " Plugged"
                         " Gtags"
                         " Abbrev"
                         " Fill"
                         " Guide"
                         " pair"
                         " yas"
                         " MRev"
                         " FN"
                         " Fly"
                         " MML"
                         " WK"
                         " ARev"
                         " GitGutter"
                         " ElDoc"))))

(use-package git-gutter
  :config (global-git-gutter-mode +1)
  :bind (("C-c m ]" . git-gutter:next-hunk)
         ("C-c m [" . git-gutter:previous-hunk)
         ("C-c m ." . git-gutter:stage-hunk)
         ("C-c m ," . git-gutter:revert-hunk)))

(use-package which-key
  :init (which-key-mode))

(use-package ag
  :bind (("C-c m a" . ag-project)))

(use-package flyspell
  :config
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'message-mode-hook 'flyspell-mode)
  ;; unbind C-; since we use it for iedit
  (define-key flyspell-mode-map (kbd "C-;") nil))

(use-package bbdb
  :config
  (when (or (file-exists-p "~/.emacs.d/bbdb")
            (file-exists-p "~/.bbdb"))
    (bbdb-initialize 'gnus 'message)
    (bbdb-mua-auto-update-init 'gnus 'message)
    (setq bbdb-mua-update-interactive-p '(create . create)
          bbdb-complete-mail-allow-cycling t
          bbdb-completion-display-record nil
          bbdb-update-records-p 'create
          ;; bbdb-pop-up-window-size 0
          bbdb-message-all-addresses t
          bbdb-layout 'one-line
          bbdb-message-pop-up nil)

    (setq bbdb-ignore-message-alist
          '((("To" "CC") . "emacs-devel@gnu\\.org")
            (("To" "CC") . "help-gnu-emacs@gnu\\.org")
            ("From" . "notifications@github\\.com")
            ("X-Mailman-Version" . ".*")))))

(use-package jade-mode
  :mode ("\\.jade$" . jade-mode))

(use-package sws-mode
  :mode ("\\.styl$" . sws-mode))

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.djhtml?\\'" . web-mode)
         ("\\.tpl" . web-mode)
         ("\\.jsx" . web-mode)
         )
  :config
  (defun m/current-buffer-django-p ()
    (save-excursion
      (search-forward-regexp "{% base\\|{% extends\\|{% if\\|{% for\\|{% include\\|{% block\\|{% csrf_token %}\\|{% url\\|{% verbatim\\|{{ "
                             nil
                             t)))
  (setq web-mode-engines-alist
        '(("django". "\\.djhtml")
          ("django" . m/current-buffer-django-p)
          ("php" . "\\.php")))
  (setq web-mode-content-types-alist
        '(("jsx"  . "\\.jsx")))
  (define-key web-mode-map (kbd "C-;") nil)
  (define-key web-mode-map (kbd "C-c C-h") nil)
  (setq-default web-mode-markup-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-indentation nil)
  (add-to-list 'web-mode-indentation-params '("case-extra-offset" . nil)))

(use-package nyan-mode
  :if window-system
  :config
  (when window-system
    (nyan-mode)
    (setq nyan-bar-length 10)))

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char)
   ("C-c m SPC" . avy-pop-mark)
   ("M-g g" . avy-goto-line)))

(use-package ov)

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#2aa198")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#b58900")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#268bd2")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#dc322f")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#859900")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#268bd2")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#cb4b16")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#d33682")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#839496"))

(use-package erc
  :config
  (defun m/erc-ansi-colors ()
    "For ansi color escape sequences"
    (ansi-color-apply-on-region (point-min) (point-max)))
  (add-hook 'erc-insert-modify-hook 'm/erc-ansi-colors)
  (erc-notifications-mode)
  (setq erc-track-showcount t)
  (setq erc-track-shorten-cutoff 8)
  (setq erc-track-exclude-types
        '("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353"))
  (use-package erc-hl-nicks))

(use-package dts-mode
  :pin gnu)

(use-package numbers
  :ensure nil
  :load-path "lisp/")

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 800))

(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-mode)))

;; (use-package graphviz-dot-mode)

(use-package go-mode
  :init
  (use-package go-eldoc
    :init
    (add-hook 'go-mode-hook 'go-eldoc-setup)))

(use-package go-autocomplete)

(use-package python
  :init
  (use-package blacken
    :custom
    (blacken-executable "~/.emacs.d/bin/black-uv"))
  :bind
  (("C-c m m k" . blacken-buffer))

  :config
  (setq python-fill-docstring-style 'django)

  ;; (add-hook 'python-mode-hook
  ;;           #'(lambda ()
  ;;               (setq autopair-handle-action-fns
  ;;                     (list #'autopair-default-handle-action
  ;;                           #'autopair-python-triple-quote-action))))
  )

(setq m/pyright-uvx-command
      '("uvx" "--from" "pyright==1.1.403" "pyright-langserver" "--" "--stdio"))

(use-package eglot
  :hook
  ((python-mode . eglot-ensure)
   (rust-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs `(python-mode . ,m/pyright-uvx-command))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  ;; (setq eglot-report-progress nil)
  )

(use-package nginx-mode)

;; (use-package helm-cscope
;;   :init
;;   (add-hook 'python-mode-hook 'helm-cscope-mode)
;;   :bind
;;   (:map helm-cscope-mode-map
;;         ("M-." . helm-cscope-find-global-definition)
;;         ("M-*" . helm-cscope-pop-mark)))

(use-package gnus
  :config
  (setq shr-use-fonts nil)
  (setq gnus-treat-fill-long-lines nil)
  (define-key gnus-article-mode-map (kbd "C-c m m l") 'gnus-article-fill-long-lines)

  (defun m/ov-whole-buffer (regexp color-spec)
    (let (face-plist)
      (if (stringp color-spec)
          (setq face-plist `(:foreground ,color-spec))
        (setq face-plist `(:foreground ,(car color-spec)
                                       :background ,(cadr color-spec))))
      (ov-set (ov-regexp regexp)
              (point-min)
              (point-max)
              'face face-plist)))

  (defun m/overlays-for-mailing-lists ()
    (interactive)
    (let ((regspecs '(("\\[RFC.*\\]" "chartreuse1")
                      ("\\[PATCH.*\\]" "dark turquoise")
                      ("\\b[^[:blank:]]+: " "tomato1")
                      ("\\[GIT PULL\\]" "red4")
                      ("\\bion\\b" ("dark green" "green"))
                      ("\\biommu/arm-smmu\\b" ("dark red" "green"))
                      ("\\biommu/io-pgtable\\b" ("dark red" "green"))
                      ("\\biommu/dma\\b" ("dark red" "green")))))
      (dolist (r regspecs)
        (m/ov-whole-buffer (car r) (cadr r))))))

(use-package indent-hints
  :ensure nil
  :load-path "lisp/indent-hints-mode"
  :config
  (setq indent-hints-profile-switching-enabled t
        indent-hints-ignore-c-styles '("linux"))
  ;; we add our hook to both c-mode-hook and c-mode-common-hook to get
  ;; ignore-c-styles working.
  (add-hook 'c-mode-hook 'indent-hints-activate t)
  (add-hook 'c-mode-common-hook 'indent-hints-activate t)
  (add-hook 'dts-mode-hook 'indent-hints-activate))

(use-package conf-mode
  :mode (("/crontab.*$" . conf-mode)
         ("_defconfig$" . conf-mode)
         ("/.gitconfig". conf-mode)))

(use-package markdown-mode
  :mode (("\\.md" . markdown-mode)
         ("\\.markdown" . markdown-mode)))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package cmake-mode
  :mode (("\\.cmake$" . cmake-mode)
         ("CMakeLists\\.txt$" . cmake-mode)))

(use-package sh-script
  :mode ("\.install" . sh-mode))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'js-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-idle-change-delay 3)
  (setq flycheck-idle-buffer-switch-delay 3)

  ;; use local eslint from node_modules before global
  ;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (defun m/add-eslint-extra-config ()
    "If dominating file eslint-extra.json is found, add it to the
eslint command line args with -c"
    (let* ((extra-config-filename "eslint-extra.json")
           (extra-config-file (locate-dominating-file default-directory
                                                      extra-config-filename))
           (extra-config-file (and extra-config-file
                                   (expand-file-name extra-config-file))))
      (when extra-config-file
        (setq-local flycheck-eslint-args (list "-c" (concat extra-config-file
                                                            extra-config-filename))))))

  (defun my/configure-web-mode-flycheck-checkers ()
    ;; in order to have flycheck enabled in web-mode, add an entry to this
    ;; cond that matches the web-mode engine/content-type/etc and returns the
    ;; appropriate checker.
    (-when-let (checker (cond
                         ((string= web-mode-content-type "jsx")
                          'javascript-eslint)))
      (flycheck-mode)
      (flycheck-select-checker checker)))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook #'m/add-eslint-extra-config)
  (add-hook 'web-mode-hook #'my/configure-web-mode-flycheck-checkers)

  :config
  (use-package flycheck-package
    :config
    (eval-after-load 'flycheck
      '(flycheck-package-setup))))

(use-package gist)

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode
  :init
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package restclient)

(use-package json-mode)

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook 'show-paren-mode))

;;; you also need a ~/.lein/profiles.clj with something like:
;;; {:user {:plugins [[cider/cider-nrepl "0.10.0-SNAPSHOT"]]}}
;;; more setup info at https://github.com/clojure-emacs/cider
(use-package cider
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package company
  :diminish company-mode
  :config
  (setq company-backends (-remove (lambda (el) (eq el 'company-dabbrev))
                                  company-backends))
  (global-company-mode))

(use-package company-go
  :init
  (add-hook 'go-mode-hook 'company-mode))

(use-package rust-mode
  :init
  (use-package cargo
    :init
    (add-hook 'rust-mode-hook 'cargo-minor-mode)
    :diminish cargo-minor-mode))

(use-package toml-mode)

(use-package systemd)

(use-package jumbobuffer
  :load-path "lisp/jumbobuffer"
  :ensure nil)

(use-package calc
  :bind
  (("C-c m C-c" . calc)))

(use-package casual
  :after (calc)
  :bind (:map
         calc-mode-map
         ("C-o" . casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-o" . casual-calc-tmenu)
         :map
         dired-mode-map
         ("C-o" . casual-dired-tmenu)
         :map
         isearch-mode-map
         ("C-o" . casual-isearch-tmenu)))

(use-package elisp-mode
  :ensure nil
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  :bind
  (("C-c M-s ." . isearch-forward-symbol-at-point)))

(use-package default-text-scale
  :bind
  (("C-M-=" . default-text-scale-increase)
   ("C-M--" . default-text-scale-decrease)))

(use-package debbugs)

(use-package git-timemachine
  :bind
  (("C-c m C-m" . git-timemachine)))

;;; override `vc-git-find-file-hook' due to
;;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559
(require 'vc-git)
(defun vc-git-find-file-hook () "Just no. http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559")

(use-package chess)

(use-package calendar
  :bind
  (("C-c m m C" . calendar)))

(use-package git-link)

(use-package celestial-mode-line
  :config
  (setq calendar-latitude 33.004174)
  (setq calendar-longitude -117.047074)
  (setq calendar-location-name "Poway, CA")
  (setq global-mode-string '("" celestial-mode-line-string " " display-time-string))
  (celestial-mode-line-start-timer))

(use-package groovy-mode)

(use-package lorem-ipsum)

(use-package pyvenv)
(use-package emacs-pager
  :ensure nil
  :load-path "lisp/emacs-pager"
  :mode ("\\.emacs-pager$" . emacs-pager-mode))

(use-package dart-mode)
;; (use-package lsp-dart
;;   :ensure t
;;   :hook (dart-mode . lsp))

(use-package counsel)

(use-package swiper
  :bind (:map
         isearch-mode-map
         ("M-i" . swiper-from-isearch)))

(use-package ivy
  :init
  ;; advice to prevent dynamic exhibit delay for C-n/C-p
  ;; https://github.com/abo-abo/swiper/issues/1218#issuecomment-962516670
  (defvar +ivy--queue-last-input nil)
  (defun +ivy-queue-exhibit-a(f &rest args)
    (if (equal +ivy--queue-last-input (ivy--input))
        (ivy--exhibit)
      (apply f args))
    (setq +ivy--queue-last-input (ivy--input)))
  (advice-add 'ivy--queue-exhibit :around #'+ivy-queue-exhibit-a)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

;; GUI dialog for printing (print-buffer) and friends
(setq lpr-command "gtklp")
(setq ps-lpr-command "gtklp")

(use-package quick-clone
  :ensure nil
  :load-path "lisp/quick-clone")

(use-package olivetti
  :bind
  (("C-c m m m o" . olivetti-mode)))

(use-package sudo-edit)

;;; Add a "merge" operation to ediff
;;; https://stackoverflow.com/a/29757750/209050
(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

(use-package helpful)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-definitions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("C-~"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package posframe)

;;; Note: relevant ~/.authinfo entries are required, i.e.
;; machine api.openrouter.com login openrouter-api-key password ****
(use-package gptel
  :config
  (require 'gptel-context)
  :bind (("C-c m G g" . (lambda () (interactive) (switch-to-buffer (gptel "*OpenRouter*"))))
         ("C-c m G a" . gptel-add)
         ("C-c m G c" . gptel-context-remove-all)
         (:map
          gptel-mode-map
          ("C-t" . gptel-menu)))
  :config

  ;; Function to retrieve the API key from ~/.authinfo
  (defun get-authinfo-secret (host user)
    "Retrieve a secret from ~/.authinfo given a HOST and USER."
    (let ((auth-info (auth-source-search
                      :host host
                      :user user
                      :require '(:user :secret))))
      (if auth-info
          (let ((secret (plist-get (car auth-info) :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))
        nil)))

  (defun get-openrouter-api-key ()
    "Retrieve the OpenRouter API key from ~/.authinfo."
    (get-authinfo-secret "api.openrouter.com" "openrouter-api-key"))

  ;; ★ default model and backend ★
  (setq gptel-model 'qwen/qwen3-235b-a22b-2507
        gptel-backend (gptel-make-openai "OpenRouter"
                        :host "openrouter.ai"
                        :endpoint "/api/v1/chat/completions"
                        :stream t
                        :key (get-openrouter-api-key)
                        :models '(anthropic/claude-3.7-sonnet
                                  anthropic/claude-3.7-sonnet:thinking
                                  anthropic/claude-opus-4
                                  anthropic/claude-sonnet-4
                                  deepseek/deepseek-chat-v3-0324
                                  deepseek/deepseek-chat-v3-0324:free
                                  deepseek/deepseek-r1-0528
                                  deepseek/deepseek-r1-0528:free
                                  google/gemini-2.5-flash
                                  google/gemini-2.5-pro
                                  moonshotai/kimi-k2
                                  moonshotai/kimi-k2:free
                                  openai/gpt-4.1
                                  openai/gpt-4.1-mini
                                  openai/gpt-4o
                                  openai/gpt-4o-2024-11-20
                                  openai/gpt-4o-mini
                                  openai/gpt-5
                                  openai/gpt-5-chat
                                  openai/gpt-5-mini
                                  openai/gpt-5-nano
                                  openai/gpt-oss-120b
                                  openai/gpt-oss-20b
                                  openai/o1
                                  openai/o1-mini
                                  openai/o3-mini
                                  openai/o3-mini-high
                                  openai/o3-pro
                                  qwen/qwen3-235b-a22b-2507
                                  qwen/qwen3-235b-a22b-2507:free
                                  qwen/qwen3-235b-a22b-thinking-2507
                                  qwen/qwen3-coder
                                  qwen/qwen3-coder:free
                                  x-ai/grok-3-mini-beta))))

(defun m/gptel-add-screenshot-from-clipboard ()
  "Capture screenshot from clipboard and add to gptel context."
  (interactive)
  (let ((temp-file (make-temp-file "gptel-screenshot-" nil ".png")))
    (shell-command (format "xclip -selection clipboard -t image/png -o > %s" temp-file))
    (gptel-add-file temp-file)))

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick"))

(use-package indent-bars
  :config
  (require 'indent-bars-ts) 		; not needed with straight
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                               if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-base-mode yaml-mode) . indent-bars-mode))

(use-package boxquote)

(use-package autosave-notes
  :load-path "~/.emacs.d/lisp"
  :bind (("C-c m n" . autosave-notes-open-notes-buffer))
  :config
  (autosave-notes-mode 1))

(when window-system
  (use-package modern-fringes
    :straight (modern-fringes :type git :host github :repo "SpecialBomb/emacs-modern-fringes")
    :init
    ;; Adjust fringe bitmaps to match the background color
    (modern-fringes-invert-arrows)
    ;; Enable modern-fringes globally
    (modern-fringes-mode 1)))

(defun m/compilation-mode-hook ()
  (let ((current-compilation-buffer (current-buffer)))
    (run-at-time "0.1 sec" nil (lambda (buf)
                                 (with-current-buffer buf
                                   (message "Scrolling in %s" buf)
                                   (goto-char (point-max))))
                 current-compilation-buffer)))

(add-hook 'compilation-mode-hook 'm/compilation-mode-hook)

(use-package treesit
  :ensure nil  ; it's built-in
  :mode (("\\.ts" . typescript-ts-mode))
  :preface
                                        ; https://github.com/mickeynp/combobulate?tab=readme-ov-file#complete-example-with-tree-sitter-grammar-installation
  (defun m/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             ;; Note the version numbers. These are the versions that
             ;; are known to work with Combobulate *and* Emacs.
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (message "Installing missing treesit grammar %s from %s @ %s"
                 (car grammar)
                 (cadr grammar)
                 (cddr grammar))
        (treesit-install-language-grammar (car grammar)))))
  :config
  (m/setup-install-grammars))

(use-package kotlin-mode)

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(use-package-nope jtsx
  :ensure t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  ;; (typescript-ts-mode-indent-offset 2)
  ;; (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  ;; (jtsx-enable-jsx-electric-closing-element t)
  ;; (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  ;; (jtsx-enable-jsx-element-tags-auto-sync nil)
  ;; (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c j u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c j d n") 'jtsx-delete-jsx-node)
    (define-key mode-map (kbd "C-c j d a") 'jtsx-delete-jsx-attribute)
    (define-key mode-map (kbd "C-c j t") 'jtsx-toggle-jsx-attributes-orientation)
    (define-key mode-map (kbd "C-c j h") 'jtsx-rearrange-jsx-attributes-horizontally)
    (define-key mode-map (kbd "C-c j v") 'jtsx-rearrange-jsx-attributes-vertically))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map))

(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c h" . consult-history)
         ;; ("C-c k" . consult-kmacro)
         ;; ("C-c m" . consult-man)
         ;; ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)

         ;; C-x bindings in `ctl-x-map'
         ;; ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ;; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ;; ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x C-b" . consult-buffer)

         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)

         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ;; ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)

         ;; M-s bindings in `search-map'
         ;; ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ;; ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ;; ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ;; ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ;; ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)  ;; orig. next-matching-history-element
         ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; (setq completion-styles '(substring basic))
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic)))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :bind
  (:map
   vertico-map
   ("C-l" . vertico-directory-up)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package minuet
  :ensure t
  :bind
  (("M-i" . #'minuet-show-suggestion)          ;; use overlay for completion
   ;; ("M-y" . #'minuet-complete-with-minibuffer); use minibuffer for completion
   ; ("C-c m" . #'minuet-configure-provider)
   :map minuet-active-mode-map
   ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
   ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
   ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
   ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
   ;; Accept the first line of completion, or N lines with a numeric-prefix:
   ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
   ("M-a" . #'minuet-accept-suggestion-line)
   ("M-e" . #'minuet-dismiss-suggestion))

  ;; :init
  ;; if you want to enable auto suggestion.
  ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
  ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)

  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
                                        ;(setq minuet-provider 'openai-fim-compatible)
                                        ;(minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)
  (setq minuet-provider 'openai-compatible)
  (setq minuet-request-timeout 2.5)
  (setq minuet-auto-suggestion-throttle-delay 1.5) ;; Increase to reduce costs and avoid rate limits
  (setq minuet-auto-suggestion-debounce-delay 0.6) ;; Increase to reduce costs and avoid rate limits

  (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key "OPENROUTER_API_KEY")
  (plist-put minuet-openai-compatible-options :model "qwen/qwen3-coder")

  ;; Prioritize throughput for faster completion
  (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 56)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9))

(use-package commitothy
  :load-path "~/.emacs.d/lisp"
  :custom
  (commitothy-model "gpt-5-chat")
  :bind
  (:map
   git-commit-mode-map
   ("C-c C-l" . commitothy-write-commit-message)
   ("TAB" . commitothy-improve-commit-message)))


;;; These lines should be last:
;; some keybindings
(m/l "my-keybindings.el")

(run-hooks 'm/init-complete-hook)
