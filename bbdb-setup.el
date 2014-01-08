(require 'bbdb-loaddefs)
(require 'bbdb)

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
      '((("To" "CC") . "emacs-devel@gnu\\.org")))

(require 'nnir)
;; (setq bbdb-mua-update-interactive-p '(query . create))
