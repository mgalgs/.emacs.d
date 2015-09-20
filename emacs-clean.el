;;; From: http://nic.ferrier.me.uk/blog/2012_07/emacs-packages-for-programmers

;; Usage: mkdir myclean ; cd myclean ; emacs -Q -nw -l ~/emacs-clean.el
;; (or use emacs_clean.sh)

(setq package-user-dir
      (concat
       default-directory
       "elpa"))
(message "Using package-user-dir: %s" package-user-dir)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(package-refresh-contents)
