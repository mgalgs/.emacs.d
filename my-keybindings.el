;;; A few miscellaneous general keybindings: ;;;

;; Typical prefixes:
;; o C-m
;; o C-m m


;; using a macro for location-independence. (this is pointless on its
;; own (other pieces of my config depend on stuff being located at
;; ~/.emacs.d) but oh well)
(defmacro my-visit-keybindings-file-maker ()
  "Defines a function to visit my-keybindings.el"
  (let ((keybindings-file-name load-file-name))
    `(defun my-visit-keybindings-file ()
       "Visits my-keybindings.el"
       (interactive)
       (find-file ,keybindings-file-name))))

;; define my-visit-keybindings-file by calling
;; my-visit-keybindings-file-maker
(my-visit-keybindings-file-maker)

(global-set-key (kbd "C-c m k") 'my-visit-keybindings-file)
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<S-f5>") 'revert-and-goto-end-of-buffer)

(global-set-key (kbd "M-/") 'dabbrev-expand)
(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<S-f6>") 'previous-error)
(global-set-key (kbd "<f7>") 'compile)

(global-set-key (kbd "<C-f7>") 'rebuild-the-unit-tests)

(global-set-key "\C-cmw" 'woman)
(global-set-key (kbd "<M-up>") 'my-increment-number-decimal)
(global-set-key (kbd "<M-down>") 'my-decrement-number-decimal)

(global-set-key (kbd "M-P") 'my-up-a-line)
(global-set-key (kbd "M-N") 'my-down-a-line)
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)

(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

(global-set-key "\M-Y" 'yank-pop-forwards)
(global-set-key [(shift delete)] 'clipboard-kill-region)
(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cv" 'org-export-visible)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key (kbd "C-c k") 'browse-kill-ring)
(global-set-key (kbd "C-c m d") 'my-lookup-current-word)
(global-set-key (kbd "<f8>") 'my-toggle-tab-width-setting)
(global-set-key (kbd "C-c m s") 'my-search-all-buffers)

(global-set-key (kbd "C-c r r") 'get-rfc-view-rfc)
(global-set-key (kbd "C-c r .") 'get-rfc-view-rfc-at-point)
(global-set-key (kbd "C-c r g") 'get-rfc-grep-rfc-index)
(global-set-key (kbd "C-c r l") 'get-rfc-list-downloaded-rfcs)

(global-set-key (kbd "C-c m i") 'kill-where-i-am)
(global-set-key (kbd "C-c m g") 'grep-what-im-on)
(global-set-key (kbd "C-c m l") 'my-kill-last-message)
(global-set-key (kbd "C-c m o") 'browse-url)
(global-set-key (kbd "C-c m r") 'align-regexp)
(global-set-key (kbd "C-c m x") 'my-xdg-open-each-in-region)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(global-set-key "\C-xp" 'my-previous-window)

(global-set-key (kbd "C-c f") 'iy-go-to-char)
(global-set-key (kbd "C-c F") 'iy-go-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-char-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-char-continue-backward)

(global-set-key (kbd "C-M-S-SPC") 'one-key-menu-toplevel)
(global-set-key (kbd "C-c m SPC") 'one-key-menu-toplevel)
(global-set-key (kbd "M-S") 'lazy-search-menu)

(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(define-key rfcview-mode-map "j" 'pageview-goto-next-page-break)
(define-key rfcview-mode-map "k" 'pageview-goto-previous-page-break)

;; global:
(global-set-key "\C-cgn" 'my-gtags-next-result)
(global-set-key (kbd "C-M-*") 'gtags-pop-stack)
(global-set-key (kbd "\C-cm*") 'gtags-pop-stack)
(global-set-key (kbd "C-M-<") 'gtags-find-tag)
(global-set-key (kbd "\C-cm,") 'gtags-find-tag)
(global-set-key (kbd "C-M->") 'gtags-find-tag-from-here)
(global-set-key (kbd "\C-cm.") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-c m m G") 'my-gtags-update-tags-file)
(global-set-key (kbd "C-c m m g") 'my-gtags-update-tags-file-incrementally)

(define-key gtags-select-mode-map "n" 'next-line)
(define-key gtags-select-mode-map "p" 'previous-line)
(define-key gtags-select-mode-map "q" 'bury-buffer)

;; doc view
(define-key doc-view-mode-map "j" 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map "k" 'doc-view-previous-line-or-previous-page)

;; {wo,}man
(add-hook 'Man-mode-hook '(lambda ()
			    (interactive)
			    (define-key Man-mode-map "f" 'scroll-up-command)
			    (define-key Man-mode-map "w" 'scroll-down-command)))
(add-hook 'woman-mode-hook '(lambda ()
			      (define-key woman-mode-map "f" 'scroll-up-command)
			      (define-key woman-mode-map "w" 'scroll-down-command)))

;; diff
(define-key diff-mode-map "q" 'bury-buffer)

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

(global-set-key "\C-ci" 'my-go-to-corresponding-header-or-implementation-file)

(global-set-key "\C-x>" (lambda () (interactive) (other-window 1) (end-of-buffer)))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-c m =") 'er/expand-region)

;; magit
(global-set-key (kbd "C-c m t") 'magit-status)
(global-set-key (kbd "C-c m c") 'magit-show-commit)
(global-set-key (kbd "C-c m m c") 'my-show-commit-at-point)
(define-key magit-mode-map (kbd "u") 'magit-goto-parent-section)

(global-set-key (kbd "C-c m n") 'my-visit-init-file)

(eval-after-load "gnus"
  '(define-key gnus-article-mode-map (kbd "C-c m m l") 'gnus-article-fill-long-lines))

(global-set-key (kbd "C-c m m d") 'mark-defun)
(global-set-key (kbd "C-c m ;") 'iedit-mode)
(define-key iedit-lib-keymap (kbd "C-c m '") 'iedit-toggle-unmatched-lines-visible)

(global-set-key (kbd "C-c m `") 'my-recompile)

(global-set-key (kbd "C-c m p") 'pwd)
(global-set-key (kbd "C-c m m k") 'textmate-clear-cache)
(global-set-key (kbd "C-c m m o") 'my-occur-region-or-symbol-at-point)
(global-set-key (kbd "M-s h .") 'highlight-symbol-at-point)
(global-set-key (kbd "M-s h n") 'highlight-symbol-next)
(global-set-key (kbd "M-s h p") 'highlight-symbol-prev)
(global-set-key (kbd "M-s h a") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-S-t") 'transpose-words)
(global-set-key (kbd "C-c m m v") 'vc-git-grep)

(global-set-key (kbd "C-c h") 'helm-mini)

(global-set-key (kbd "C-c m b") 'eval-buffer)
