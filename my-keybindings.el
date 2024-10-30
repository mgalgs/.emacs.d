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

(global-set-key "\C-cmw" 'man)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "C-c m o") 'browse-url)

(global-set-key (kbd "C-c m m G") 'm/gtags-update-tags-file)
(global-set-key (kbd "C-c m m g") 'm/gtags-update-tags-file-incrementally)


(global-set-key "\C-x>" (lambda () (interactive) (other-window 1) (end-of-buffer)))

(global-set-key (kbd "C-c m n") 'm/visit-init-file)
(global-set-key (kbd "C-c m m s") (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key (kbd "C-c m m t") (lambda () (interactive) (find-file "~/notes/notes.org")))

(global-set-key (kbd "C-c m p") 'pwd)

(global-set-key (kbd "C-c m b") (lambda ()
                                  (interactive)
                                  (eval-buffer)
                                  (message "eval'd buffer")))


;; (global-set-key (kbd "C-S-o") 'm/open-line-after-this-line)
;; (global-set-key (kbd "C-M-S-o") 'm/open-line-before-this-line)

(global-set-key (kbd "C-c m 4") (lambda ()
                                  (interactive)
                                  (string-rectangle (region-beginning)
                                                    (region-end)
                                                    "    ")))
(global-set-key (kbd "C-c m m y") 'bury-buffer)



(global-set-key (kbd "C-c m m e") 'm/overlays-for-mailing-lists)

;;; Some stuff to make UHK life easier
(global-set-key (kbd "C-x SPC") 'ido-switch-buffer)
(global-set-key (kbd "M-h") 'backward-word)
(global-set-key (kbd "C-M-h") 'backward-sexp)
;; Remap C-h prefix to C-c C-h so that I can map C-h to backward-char
(global-set-key (kbd "C-c C-h") help-map)
(global-set-key (kbd "C-h") 'backward-char)
