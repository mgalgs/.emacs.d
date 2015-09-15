;;; A few miscellaneous general keybindings: ;;;

;; Typical prefixes:
;; o C-m
;; o C-m m


;; using a macro for location-independence. (this is pointless on its
;; own (other pieces of my config depend on stuff being located at
;; ~/.emacs.d) but oh well)
(defmacro m/visit-keybindings-file-maker ()
  "Defines a function to visit m/keybindings.el"
  (let ((keybindings-file-name load-file-name))
    `(defun m/visit-keybindings-file ()
       "Visits m/keybindings.el"
       (interactive)
       (find-file ,keybindings-file-name))))

;; define m/visit-keybindings-file by calling
;; m/visit-keybindings-file-maker
(m/visit-keybindings-file-maker)

(global-set-key (kbd "C-c m k") 'm/visit-keybindings-file)
(global-set-key (kbd "<f5>") 'revert-buffer)

(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<f7>") 'compile)

(global-set-key "\C-cmw" 'woman)
(global-set-key (kbd "<M-up>") 'm/increment-number-decimal)
(global-set-key (kbd "<M-down>") 'm/decrement-number-decimal)

(global-set-key (kbd "M-P") 'm/up-a-line)
(global-set-key (kbd "M-N") 'm/down-a-line)
(global-set-key (kbd "C-S-l") 'm/horizontal-recenter)

(global-set-key (kbd "C-c m d") 'm/lookup-current-word)
(global-set-key (kbd "<f8>") 'm/toggle-tab-width-setting)
(global-set-key (kbd "C-c m s") 'm/search-all-buffers)

(global-set-key (kbd "C-c m i") 'm/kill-where-i-am)
(global-set-key (kbd "C-c m m i") 'm/kill-where-i-am-relative-to-gitroot)
(global-set-key (kbd "C-c m g") 'm/grep-what-im-on)
(global-set-key (kbd "C-c m l") 'm/kill-last-message)
(global-set-key (kbd "C-c m o") 'browse-url)
(global-set-key (kbd "C-c m x") 'm/xdg-open-each-in-region)

(global-set-key (kbd "C-c m m G") 'm/gtags-update-tags-file)
(global-set-key (kbd "C-c m m g") 'm/gtags-update-tags-file-incrementally)

(global-set-key "\C-ci" 'm/go-to-corresponding-header-or-implementation-file)

(global-set-key "\C-x>" (lambda () (interactive) (other-window 1) (end-of-buffer)))

(global-set-key (kbd "C-c m n") 'm/visit-init-file)
(global-set-key (kbd "C-c m m n") (lambda () (interactive) (switch-to-buffer "*notes*")))
(global-set-key (kbd "C-c m m s") (lambda () (interactive) (switch-to-buffer "*scratch*")))

(global-set-key (kbd "C-c m `") 'm/recompile)
(global-set-key (kbd "C-c m m `") 'm/open-compilation-buffer)

(global-set-key (kbd "C-c m p") 'pwd)
(global-set-key (kbd "C-c m m o") 'm/occur-region-or-symbol-at-point)

(global-set-key (kbd "C-c m b") (lambda ()
                                  (interactive)
                                  (eval-buffer)
                                  (message "eval'd buffer")))

(global-set-key (kbd "C-c m C-x C-e") 'eval-and-replace)

(global-set-key (kbd "C-S-o") 'm/open-line-after-this-line)
(global-set-key (kbd "C-M-S-o") 'm/open-line-before-this-line)

(global-set-key (kbd "C-c m 4") (lambda ()
                                  (interactive)
                                  (string-rectangle (region-beginning)
                                                    (region-end)
                                                    "    ")))
(global-set-key (kbd "C-c m m y") 'bury-buffer)

(global-set-key (kbd "C-h e") 'm/view-and-switch-to-echo-area-messages)

(global-set-key (kbd "C-c m M-/") 'm/expand-file-name-at-point)

(global-set-key (kbd "C-c m m e") 'm/overlays-for-mailing-lists)

(global-set-key (kbd "C-v") 'm/smooth-scroll-down)
(global-set-key (kbd "M-v") 'm/smooth-scroll-up)

(global-set-key (kbd "C-c m m u") 'm/underline-previous-line)
