(require 'ldap)
(require 'eudc)

(setq eudc-default-return-attributes nil
      eudc-strict-return-matches nil)

(setq ldap-ldapsearch-args (quote ("-tt" "-LLL" "-x")))
(setq eudc-inline-query-format '((firstname)
                                 (name)
                                 (firstname name)
                                 (email)))

(load-file "~/eudc-private.el")

;;; eudc-private.el sets these:
;; (setq ldap-host-parameters-alist
;;       (quote (("some-server" base "ou=people,o=somegroup"))))
;; (eudc-set-server "some-server" 'ldap t)
;; (setq eudc-server-hotlist '(("some-server" . ldap)))
;; (setq eudc-inline-expansion-servers 'hotlist)
;; (setq eudc-inline-expansion-format
;;       '("%s %s <%s>" firstname name special-email-thingy))

(defun enz-eudc-expand-inline()
  (interactive)
  (move-end-of-line 1)
  (insert "*")
  (unless (condition-case nil
              (eudc-expand-inline)
            (error nil))
    (backward-delete-char-untabify 1)))

;; Adds some hooks

(eval-after-load "message"
  '(define-key message-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "sendmail"
  '(define-key mail-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
(eval-after-load "post"
  '(define-key post-mode-map (kbd "TAB") 'enz-eudc-expand-inline))
