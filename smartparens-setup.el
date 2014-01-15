(require 'smartparens)
(setq sp-navigate-reindent-after-up nil)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

;; from
;; https://github.com/Fuco1/smartparens/issues/286#issuecomment-32324817
(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))
;; from
;; https://github.com/Fuco1/smartparens/issues/286#issuecomment-32324743
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

(sp-local-pair '(message-mode org-mode git-commit-mode) "`" "'")

;; don't pair "'" if we're at the end of a word (like when typing an
;; apostrophe)
(sp-pair "'" nil :unless '(sp-point-after-word-p))
