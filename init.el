(require 'server)
(unless (server-running-p)
  (server-start))

(defun m/l (file)
  "loads a file from the `user-emacs-directory'"
  (load-file (concat user-emacs-directory file)))

(m/l "init-package.el")

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
  (add-to-list 'exec-path "/usr/local/bin")
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'control))

;; some general advice
(m/l "my-advice.el")

;; some utility functions
(use-package my-util
  :ensure nil
  :commands (m/suggest-commit-message-prefix)
  :load-path "lisp/"
  :bind
  (("C-c m m a" . m/add-include)
   ("C-v" . m/smooth-scroll-down)
   ("M-v" . m/smooth-scroll-up)
   ("<M-up>" . m/increment-number-decimal)
   ("<M-down>" . m/decrement-number-decimal)
   ("M-P" . m/up-a-line)
   ("M-N" . m/down-a-line)
   ("C-S-l" . m/horizontal-recenter)
   ("C-c m d" . m/lookup-current-word)
   ("<f8>" . m/toggle-tab-width-setting)
   ("C-c m s" . m/search-all-buffers)
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
   ("C-h e" . m/view-and-switch-to-echo-area-messages)
   ("C-c m M-/" . m/expand-file-name-at-point)
   ("C-v" . m/smooth-scroll-down)
   ("M-v" . m/smooth-scroll-up)
   ("C-c m m u" . m/underline-previous-line)
   ("C-c m C-y" . m/get-primary)
   ("M-Y" . m/yank-pop-forwards)))

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
      mouse-wheel-mode nil)

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

(load-theme 'wombat)
;;; wombat sets a foreground color for the `region' face, so we need to
;;; make it unspecified so that foreground colors don't get wiped out
(set-face-attribute 'region nil :background "MediumPurple4" :foreground 'unspecified)
(set-cursor-color "#ff8844")
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
  :config
  (m/l "init-gnu-global.el")
  (setq ggtags-global-window-height 30))

(use-package whitespace
  :init
  (add-hook 'c-mode-common-hook 'whitespace-mode)
  (add-hook 'python-mode-hook (lambda ()
                                (set (make-local-variable 'whitespace-style)
                                     '(face trailing lines-tail empty indentation::space))
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
  (use-package ox-reveal)
  (use-package htmlize))

(use-package ido
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
	  "^\*Pymacs\*" "*Async Shell Command*"
	  "^\.newsrc-dribble"
	  "^\*GTAGS SELECT\*"))
  (use-package ido-vertical-mode
    :init
    (ido-vertical-mode 1)
    :config
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(use-package helm
  :bind
  (("C-x C-b" . helm-buffers-list)
   ("C-x r h" . helm-bookmarks))
  :init
  (require 'helm)
  (require 'helm-config)
  (use-package helm-ls-git
    :bind
    ("C-c m f" . helm-ls-git-ls))
  (use-package helm-swoop)
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
                  scheme-mode-hook
                  clojure-mode-hook
                  cider-repl-mode-hook))
    (add-hook hook #'enable-paredit-mode)
    (add-hook hook (lambda () (electric-pair-local-mode 0))))
  :diminish paredit-mode)

(use-package magit
  :bind
  (("C-c m t" . magit-status)
   ("C-c m T" . magit-commit)
   ("C-c m C-c" . magit-commit-popup)
   ("C-c m P" . magit-push-popup)
   ("C-c m c" . magit-show-commit)
   ("C-c m m c" . m/show-commit-at-point)
   ("C-c m :" . magit-git-command)
   ("C-c m m b" . magit-blame)
   ("C-c m m l" . m/magit-file-log))
  :init
  (use-package git-commit
    :bind
    ("C-c C-e" . m/suggest-commit-message-prefix))
  (use-package magit-gh-pulls
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
  :config
  (m/l "init-magit.el"))

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
(use-package wgrep-ack)

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

(use-package ack-and-a-half
  :load-path "lisp/ack-and-a-half"
  :ensure nil
  :config
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
  (defalias 'ack-with-args 'ack-and-a-half-with-args))

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
         ("\\.tpl" . web-mode))
  :config
  (defun m/current-buffer-django-p ()
    (save-excursion
      (search-forward-regexp "{% base\\|{% if\\|{% for\\|{% include\\|{% block\\|{% csrf_token %}\\|{% url\\|{{ "
                             nil
                             t)))
  (setq web-mode-engines-alist
        '(("django". "\\.djhtml")
          ("django" . m/current-buffer-django-p)
          ("php" . "\\.php")))
  (define-key web-mode-map (kbd "C-;") nil)
  (setq-default web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook (lambda () (electric-pair-local-mode 0))))

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

(use-package graphviz-dot-mode)

(use-package go-mode)

(use-package go-autocomplete)

(use-package python
  :config
  (setq python-fill-docstring-style 'pep-257-nn)

  (defun m/pycscope-current-repo ()
    (interactive)
    (magit-with-toplevel
      (shell-command "find . -name '*.py' > cscope.files")
      (shell-command "~/virtualenvs/pycscope/bin/pycscope -i cscope.files")))

  (add-hook 'python-mode-hook
            #'(lambda ()
                (setq autopair-handle-action-fns
                      (list #'autopair-default-handle-action
                            #'autopair-python-triple-quote-action)))))

(use-package nginx-mode)

(use-package xcscope
  :init
  (add-hook 'python-mode-hook 'cscope-minor-mode)
  :config
  (setq cscope-index-recursively t))

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
                      ("\\biommu/arm-smmu\\b" ("dark red" "green")))))
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
  :init
  (add-hook 'js-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-package
    :config
    (eval-after-load 'flycheck
      '(flycheck-package-setup))))

(use-package gist)

(use-package undo-tree
  :config (global-undo-tree-mode)
  :diminish undo-tree-mode)

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
  :diminish company-mode)

(use-package rust-mode
  :init
  (use-package racer
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    :bind
    (("TAB" . company-indent-or-complete-common))
    :diminish racer-mode)
  ;; flycheck-rust currently has a hard time with Cargo :(
  ;; disable it until it works
  ;; (use-package flycheck-rust
  ;;   :init
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  ;;   (add-hook 'rust-mode-hook 'flycheck-mode))
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
  :init
  ;; GIANT INLINE DEFINITION.  IT'S UGLY BECAUSE IT NEEDS TO BE RIPPED OUT.
  ;; This will be the default `calc-yank' implementation in the next stable
  ;; release after 10/2015.  http://emacs.stackexchange.com/a/16888/573
  (defun calc-yank (radix)
    "Yank a value into the Calculator buffer.

Valid numeric prefixes for RADIX: 0, 2, 6, 8
No radix notation is prepended for any other numeric prefix.

If RADIX is 2, prepend \"2#\"  - Binary.
If RADIX is 8, prepend \"8#\"  - Octal.
If RADIX is 0, prepend \"10#\" - Decimal.
If RADIX is 6, prepend \"16#\" - Hexadecimal.

If RADIX is a non-nil list (created using \\[universal-argument]), the user
will be prompted to enter the radix in the minibuffer.

If RADIX is nil or if the yanked string already has a calc radix prefix, the
yanked string will be passed on directly to the Calculator buffer without any
alteration."
    (interactive "P")
    (calc-wrapper
     (calc-pop-push-record-list
      0 "yank"
      (let* (radix-num
             radix-notation
             valid-num-regexp
             (thing-raw
              (if (fboundp 'current-kill)
                  (current-kill 0 t)
                (car kill-ring-yank-pointer)))
             (thing
              (if (or (null radix)
                      ;; Match examples: -2#10, 10\n(10#10,01)
                      (string-match-p "^[-(]*[0-9]\\{1,2\\}#" thing-raw))
                  thing-raw
                (progn
                  (if (listp radix)
                      (progn
                        (setq radix-num
                              (read-number
                               "Set radix for yanked content (2-36): "))
                        (when (not (and (integerp radix-num)
                                        (<= 2 radix-num)
                                        (>= 36 radix-num)))
                          (error (concat "The radix has to be an "
                                         "integer between 2 and 36."))))
                    (setq radix-num
                          (cond ((eq radix 2) 2)
                                ((eq radix 8) 8)
                                ((eq radix 0) 10)
                                ((eq radix 6) 16)
                                (t (message
                                    (concat "No radix prepended "
                                            "for invalid *numeric* "
                                            "prefix %0d.")
                                    radix)
                                   nil))))
                  (if radix-num
                      (progn
                        (setq radix-notation
                              (concat (number-to-string radix-num) "#"))
                        (setq valid-num-regexp
                              (cond
                               ;; radix 2 to 10
                               ((and (<= 2 radix-num)
                                     (>= 10 radix-num))
                                (concat "[0-"
                                        (number-to-string (1- radix-num))
                                        "]+"))
                               ;; radix 11
                               ((= 11 radix-num) "[0-9aA]+")
                               ;; radix 12+
                               (t
                                (concat "[0-9"
                                        "a-" (format "%c" (+ 86 radix-num))
                                        "A-" (format "%c" (+ 54 radix-num))
                                        "]+"))))
                        ;; Ensure that the radix-notation is prefixed
                        ;; correctly even for multi-line yanks like below,
                        ;;   111
                        ;;   1111
                        (replace-regexp-in-string
                         valid-num-regexp
                         (concat radix-notation "\\&")
                         thing-raw))
                    thing-raw)))))
        (if (eq (car-safe calc-last-kill) thing)
            (cdr calc-last-kill)
          (if (stringp thing)
              (let ((val (math-read-exprs (calc-clean-newlines thing))))
                (if (eq (car-safe val) 'error)
                    (progn
                      (setq val (math-read-exprs thing))
                      (if (eq (car-safe val) 'error)
                          (error "Bad format in yanked data")
                        val))
                  val)))))))))

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

(use-package git-timemachine)

;;; override `vc-git-find-file-hook' due to
;;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559
(require 'vc-git)
(defun vc-git-find-file-hook () "Just no. http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559")

(use-package chess)


;;; These lines should be last:
;; some keybindings
(m/l "my-keybindings.el")

(run-hooks 'm/init-complete-hook)
