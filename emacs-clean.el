;;; From: http://nic.ferrier.me.uk/blog/2012_07/emacs-packages-for-programmers

;; Usage: mkdir myclean ; cd myclean ; emacs -Q -nw -l ~/emacs-clean.el
;; (or use emacs_clean.sh)

(require 'package)

(setq package-user-dir
      (concat
       default-directory
       "pkg"))
(message "Using package-user-dir: %s" package-user-dir)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(package-refresh-contents)
