(require 'ido)			    ;ido-mode:
(ido-mode)			    ;sweet!
(setq default-truncate-lines t)	    ;no wrapping
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq scroll-step 1)			;scroll step at top/bottom of page
(setq ido-max-prospects 6)
(setq ido-ignore-buffers ;; ignore these guys
      '("\\` " "^\*Mess" "^\*Back"
        ".*Completion" "^\*Ido" "^\*trace"
        "^\*Bookmark" "^\*Compile\-Log"
        "^\*Buffer List" "^\*scratch\*"
        "^\*Shell Command Output" ;"^\*compilation\*"
        "^\*RE\-Builder\*"
		"^\*Pymacs\*" "*Async Shell Command*"
        "^\*GTAGS SELECT\*"))
