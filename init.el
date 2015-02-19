(require 'server)
(unless (server-running-p)
  (server-start))

;;; begin some misc setup. This should be first because it's
;;; distracting to switch up UI elements later during loading.
(when window-system
  (tool-bar-mode 0)
  (set-scroll-bar-mode 'right))

;; load some functions that are helpful for initialization
(load-file "~/.emacs.d/util-for-init.el")

(setq load-path
      (append
       ;; everyone under site-lisp:
       (directories-in-directory "~/.emacs.d/site-lisp")
       (list "~/.emacs.d/auto-install"
             "~/.emacs.d/python-config"
             "~/.emacs.d/site-lisp/bbdb/lisp"
             "~/.emacs.d/site-lisp/org-mode/lisp"
             "~/.emacs.d/site-lisp/org-mode/contrib/lisp"
	     ;; the site-lisp directory itself has some packages
	     "~/.emacs.d/site-lisp")
       load-path))

(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin"))

(setq custom-safe-themes t)

(setq custom-theme-load-path
      (append (directories-in-directory "~/.emacs.d/color-themes")
              '("~/.emacs.d/site-lisp/moe-theme.el")
              custom-theme-load-path))

;; some utility macros
(load-file "~/.emacs.d/my-macros.el")

;; some utility functions
(load-file "~/.emacs.d/my-util.el")

;; parse command line arguments
(setq my-switch-with-cedet-p (member "-with-cedet" command-line-args))
(setq command-line-args (delete "-with-cedet" command-line-args))

(when (file-exists-p "~/private.el")
    (load-file "~/private.el"))

;; set up python
;; (load-file "~/.emacs.d/python-config/epy-mitch.el")

;; set up cedet
(when my-switch-with-cedet-p
  (load-file "~/.emacs.d/cedet-setup.el"))

;; set up gnu global
(load-file "~/.emacs.d/gnu-global-setup.el")
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1))))

;; set up yasnippet
(load-file "~/.emacs.d/yasnippet-setup.el")

;; set up org mode
(load-file "~/.emacs.d/org-setup.el")

;; set up a bunch of auto-mode-alist stuff
(load-file "~/.emacs.d/auto-mode-alist-setup.el")

;; set up ido mode
(load-file "~/.emacs.d/ido-setup.el")
;; (load-file "~/.emacs.d/flx-setup.el")

;; set up helm
(load-file "~/.emacs.d/helm-setup.el")

;; EUDC
;; (load-file "~/.emacs.d/eudc-setup.el")

(show-paren-mode t)

; display date and time in status bar
(display-time)

;; ansi color escape codes fun:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(setq
 ;; skeleton-pair t
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
 history-length 300
 compile-command "make"
 read-file-name-completion-ignore-case t
 mouse-yank-at-point t
 initial-buffer-choice 'remember-notes)

;; backups
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.backups-emacs-saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

(setq-default
 indent-tabs-mode nil ; don't use the tab character, only spaces
 c-basic-offset 4 ; how many spaces our tab key will insert
 tab-width 4 ; default tab-width (when there are existing tabs in files)
 ;; tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
 ;; tab-stop-list '(8 16 24 32 40 48 56 64 72 80 88 96 104 112 120)
 set-mark-command-repeat-pop t ; repeated C-SPC after C-u C-SPC keeps popping the mark
 ;; c-default-style "bsd" ; for nasty brace face
 indicate-buffer-boundaries 'right
 truncate-lines t
 fill-column 75)

;; linux kernel styles
(load-file "~/.emacs.d/linux-kernel-setup.el")

;;; end misc setup

;; force some c++ keybindings
(defun my-c++-mode-keybindings-hook ()
  (local-set-key "\C-\M-e" 'end-of-defun)
  (local-set-key "\C-\M-a" 'beginning-of-defun))
(add-hook 'c++-mode-hook 'my-c++-mode-keybindings-hook)

(setq iedit-auto-recenter nil)
(require 'iedit)

;; mozrepl
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'espresso-mode-hook 'espresso-custom-setup)
;; (defun espresso-custom-setup ()
;;   (moz-minor-mode 1))

;; Line numbering
;; (global-linum-mode 1)

;; espresso mode
;; (autoload 'espresso-mode "espresso" nil t)

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
(require 'autopair)
(autopair-global-mode) ;; to enable in all buffers

;; gpg use the minibuffer for passphrase, not GUI
(setenv "GPG_AGENT_INFO" nil)

;; show column number in status bar:
(column-number-mode)

;; pkgbuild mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)

;; indent-hints minor mode
(load-file "~/.emacs.d/indent-hints-setup.el")

;; better buffer disambiguation
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; ace mode jumping
(load-file "~/.emacs.d/ace-jump-mode-setup.el")

;; batch files
(autoload 'bat-mode "bat-mode" "DOS and WIndows BAT files" t)


;; Color theming
;; (setq my-theme-to-use 'solarized-dark)
;; (setq my-theme-to-use 'solarized-light)
;; (setq my-theme-to-use 'zenburn)
(setq my-theme-to-use 'wombat)

(load-theme my-theme-to-use)
;; work-around for a hl-line-mode issue where the foreground colors
;; get lost http://stackoverflow.com/a/15746070/209050
;; put this in ~/.emacs:
;; (set-face-attribute 'highlight nil :foreground 'unspecified)

;; (require 'moe-theme)
;; (moe-light)

;; (when window-system
;;   (load-theme my-theme-to-use))


;; gist fun
(require 'gist)

;; iy go to char motion helper
;; (require 'iy-go-to-char)
;; (now using jump-char):
(require 'jump-char)

;; auto install
(require 'auto-install)

;; one key
(require 'one-key)
(load-file "~/.emacs.d/one-key-setup.el")

;; tbemail for thunderbird composition
(require 'tbemail)

;; markdown mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)

;; yaml mode
(autoload 'yaml-mode "yaml-mode" "Major mode for editing yaml files" t)

;; transpose split windows
(load-file "~/.emacs.d/auto-install/transpose.el")

(load-file "~/.emacs.d/whitespace-setup.el")

(defun my-c-mode-common-hook ()
  (interactive)
  (c-set-offset 'inextern-lang 0)
  (whitespace-mode 1))

(add-hook 'c-mode-common-hook
          'my-c-mode-common-hook)

;; browse kill ring visually
(require 'browse-kill-ring)

;; pretty control L page breaks
(require 'pp-c-l)
(pretty-control-l-mode 1)

;; here are some things that need to be here before we set some
;; *-mode-map keybindings:
(require 'doc-view)
(require 'diff-mode)

;; (load-file "~/.emacs.d/smartparens-setup.el")

(load-file "~/.emacs.d/magit-setup.el")

;; some general advice
(load-file "~/.emacs.d/my-advice.el")

;; cool-looking mode-line
;; (require 'powerline)
;; fix up the powerline modeline colors
(set-face-attribute 'mode-line nil
                    :background "turquoise4"
                    :foreground "white"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :background "grey15"
                    :foreground "grey78"
                    :box nil)

;; highlight current line
;; (global-hl-line-mode)

;; choose a web browser
;; (setq browse-url-browser-function (cond
;; 				   ((= 0 (shell-command "which google-chrome >&/dev/null")) 'browse-url-chrome)
;; 				   ((= 0 (shell-command "which chrome >&/dev/null")) 'browse-url-chrome)
;; 				   ((= 0 (shell-command "which chromium >&/dev/null")) 'browse-url-chromium)
;; 				   ((= 0 (shell-command "which chromium-browser >&/dev/null")) 'browse-url-chromium)
;; 				   ((= 0 (shell-command "which firefox >&/dev/null")) 'browse-url-firefox)))

;; auto revert (useful when switching git branches)
(global-auto-revert-mode)

;; gnus mode line notifications
(require 'gnus-notify)

;; Some advice to recenter after moving to next compile error
(defadvice next-error (after my-next-error-after
				 activate)
  "Recenter the page after next-error"
  (recenter))

;; handle ANSI color escape sequences in compilation output (like for
;; Android builds) Credit: http://stackoverflow.com/a/20788581/209050
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; some git things
(require 'gthings)
;; (gthings-init)

(load-file "~/.emacs.d/elisp-setup.el")

;; some random advice
(my-make-recentering-advice find-tag)
(my-make-recentering-advice pop-tag-mark)

;; footnote mode
(autoload 'footnote-mode "footnote" nil t)
(add-hook 'message-mode-hook 'footnote-mode)
(setq footnote-body-tag-spacing 1
      footnote-spaced-footnotes nil
      footnote-section-tag "")

(require 'lorem-ipsum)

(load-file "~/.emacs.d/mailto.el")

;; mo-git-blame (since vc-annotate hangs on me sometimes...)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(autoload 'mo-git-blame-current-for-revision "mo-git-blame" nil t)

(require 'boxquote)
(require 'thinks)

(load-file "~/.emacs.d/auto-install/uptime.el")

(load-file "~/.emacs.d/site-lisp/kconfig-mode.el")

(load-file "~/.emacs.d/which-function-mode-setup.el")

(load-file "~/.emacs.d/header-line-setup.el")

(require 'kernel-stack-trace-mode)

;; Define functions to wrap a bunch of conversion scripts in ~/scripts
(dolist (file (directory-files "~/scripts"))
  ;; all files that don't end in ~, have a `2' in the name, and one of
  ;; [hex, dec, bin]
  (when (and (not (string-match "~$" file))
	     (string-match "2" file)
	     (or (string-match "hex" file)
		 (string-match "dec" file)
		 (string-match "bin" file)))
    (my-make-shell-caller (file-name-nondirectory file))))

(require 'diffview)

(require 'expand-region)

(defmacro my-visit-init-file-maker ()
  "Defines a function to visit init.el"
  (let ((init-file-name load-file-name))
    `(defun my-visit-init-file ()
       "Visits init.el"
       (interactive)
       (find-file ,init-file-name))))

;; define my-visit-init-file by calling
;; my-visit-init-file-maker
(my-visit-init-file-maker)

(delete-selection-mode)

(load-file "~/.emacs.d/google-this-setup.el")

;; `Edit with Emacs' chrome extension:
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
(require 'edit-server)
(edit-server-start)

(require 'highlight-symbol)

;; don't open a new dired buffer when I go up a directory. Note that
;; to prevent opening a new buffer when browsing to directories
;; normally, just use `a' instead of `enter'.
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))))

(load-file "~/.emacs.d/wgrep-setup.el")

(fset 'yes-or-no-p 'y-or-n-p)

(require 'dash)

(load-file "~/.emacs.d/jedi-setup.el")

;; (load-file "~/.emacs.d/w3m-setup.el")

;; for the mighty `*'
;; (require 'evil)

(require 'litable)

(load-file "~/.emacs.d/smart-mode-line-setup.el")

(require 'transpose-frame)

(load-file "~/.emacs.d/diff-hl-setup.el")

(load-file "~/.emacs.d/guide-key-setup.el")

(load-file "~/.emacs.d/ack-and-a-half-setup.el")

(load-file "~/.emacs.d/flyspell-setup.el")

(load-file "~/.emacs.d/eldoc-setup.el")

(when (or (file-exists-p "~/.emacs.d/bbdb")
          (file-exists-p "~/.bbdb"))
  (load-file "~/.emacs.d/bbdb-setup.el"))

(load-file "~/.emacs.d/jade-mode-setup.el")

(savehist-mode 1)

(load-file "~/.emacs.d/web-mode-setup.el")

(load-file "~/.emacs.d/multiple-cursors-setup.el")

(when window-system
  (load-file "~/.emacs.d/nyan-mode-setup.el"))

(load-file "~/.emacs.d/ace-link-setup.el")

(require 's)

(require 'ov)

(load-file "~/.emacs.d/gnus-summary-overlays-setup.el")

(load-file "~/.emacs.d/rainbow-delimiters-setup.el")

(load-file "~/.emacs.d/erc-setup.el")

(require 'dts-mode)

(require 'jumbobuffer)

(require 'numbers)

(load-file "~/.emacs.d/recentf-setup.el")

(require 'dockerfile-mode)


;;; These lines should be last:
;; some keybindings
(load-file "~/.emacs.d/my-keybindings.el")

(when (file-exists-p "~/local_overrides.el")
    (load-file "~/local_overrides.el"))
