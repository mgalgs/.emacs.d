(require 'ido)                      ;ido-mode:
(ido-mode)                          ;sweet!
(setq default-truncate-lines t)             ;no wrapping
(setq ido-enable-flex-matching t) ; fuzzy matching
(setq scroll-step 1)			;scroll step at top/bottom of page
(setq ido-max-prospects 6)
(setq ido-auto-merge-work-directories-length -1) ; Don't try to find files nearby ("Searching for...")
(setq ido-ignore-buffers ;; ignore these guys
      '("\\` " "^\*Back"
        ".*Completion" "^\*Ido" "^\*trace"
        "^\*Bookmark" "^\*Compile\-Log"
        "^\*Buffer List"
        "^\*Shell Command Output" ;"^\*compilation\*"
        "^\*RE\-Builder\*"
        "^\*Pymacs\*" "*Async Shell Command*"
        "^\.newsrc-dribble"
        "^\*GTAGS SELECT\*"))

(defun mgalgs/ido-keys ()
  (define-key ido-completion-map " " 'ido-restrict-to-matches))

(add-hook 'ido-setup-hook 'mgalgs/ido-keys)
