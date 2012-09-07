(require 'bbdb-loaddefs)
(require 'bbdb)

(bbdb-initialize 'gnus 'message)
(bbdb-mua-auto-update-init 'gnus 'message)

(setq bbdb-mua-update-interactive-p '(create . create)
      bbdb-complete-mail-allow-cycling t
      bbdb-completion-display-record nil
      bbdb-update-records-p 'create
      bbdb-pop-up-window-size 10
      bbdb-message-all-addresses t
      bbdb-ignore-message-alist '(("From" . "code-review@localhost")
				  ("From" . "oit_prd@qualcomm.com")
				  ("From" . "eecomm@qualcomm.com")
				  ("From" . "code-review@qualcomm.com")
				  ("From". "quiclost@quicinc.com")
				  ("From". "jiramail@qualcomm.com")
				  ("From". "jive_admin@qualcomm.com")
				  (("To" "CC") . "mitch.special@gmail.com")
				  (("X-Mailman-Version" "X-Mailing-List") . ".*")))

(require 'nnir)
;; (setq bbdb-mua-update-interactive-p '(query . create))
