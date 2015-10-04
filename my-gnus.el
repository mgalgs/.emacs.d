;; symlink me to ~/.gnus.el

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587
                                   "mitch.special@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(setq user-mail-address "mitch.special@gmail.com")
(setq user-full-name "Mitchel Humpherys")
(setq gnus-permanently-visible-groups "INBOX")

(setq-default
 gnus-summary-line-format "%U%R%z %(%&user-date;  %-15,15f  %B%s%)\n"
 gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
 gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
 gnus-thread-sort-functions '(gnus-thread-sort-by-date))

(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-citation-line-format "On %a, %b %d %Y at %r, %f wrote:")
(setq gnus-thread-hide-subtree t)

(setq message-confirm-send t)

;; eye-candy for the summary view
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "◯ ")
  (setq gnus-sum-thread-tree-single-indent "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))

;; Reply styles
(defun m/set-style-outlook ()
  (interactive)
  (set (make-local-variable 'message-cite-style)
       message-cite-style-outlook))

(defun m/set-cite-reply-position (where)
  (set (make-local-variable 'message-cite-reply-position) where))

(defun m/set-style-traditional ()
  (interactive)
  (m/set-cite-reply-position 'traditional))

(defun m/set-style-above ()
  (interactive)
  (m/set-cite-reply-position 'above))

(defun m/set-style-below ()
  (interactive)
  (m/set-cite-reply-position 'below))

;; Sigs
(setq gnus-posting-styles
      '((".*"
         (signature "Mitch"))))

(setq gnus-completing-read-function 'gnus-emacs-completing-read)

(require 'gnus-article-treat-patch)
(setq gnus-article-patch-conditions
      '("^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@"))

;; save message by hitting `o'
(setq gnus-file-save-name 'm/gnus-patch-save-name)
(setq gnus-default-article-saver 'gnus-summary-write-to-file)
;; prevent "X-Gnus-Coding-System: -*- coding: utf-8; -*-" header:
(setq gnus-article-save-coding-system nil)
