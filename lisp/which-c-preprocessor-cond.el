;; like which-func, but for C preprocessor statements
;; Usage:
;;     (setq-default header-line-format
;;                  `((which-c-preprocessor-cond-mode ,which-c-preprocessor-cond-format)))
;;
;; TODO:
;;   - make this a package, improve docs

(defgroup which-c-preprocessor-cond nil
  "Display the current preprocessor conditional in the mode line."
  :group 'tools)

(defcustom which-c-preprocessor-cond-format
  `("["
    ;; (:propertize (:eval (which-c-preprocessor-cond))
    (:propertize which-c-preprocessor-cond-current
                 face font-lock-preprocessor-face)
    "]")
  "Format for displaying current preprocessor conditional in the mode line."
  :group 'which-c-preprocessor-cond)

(defconst which-c-preprocessor-cond-current
  '(:eval (let ((the-cond (which-c-preprocessor-cond)))
            (if the-cond
                (replace-regexp-in-string
                 "%" "%%"
                 the-cond)
              ""))))

(defun which-c-preprocessor-cond ()
  (save-excursion
    (ignore-errors
      (let ((newpoint (c-scan-conditionals -1 -1 t)))
        (goto-char newpoint)
        (s-trim-right (thing-at-point 'line t))))))

(define-minor-mode which-c-preprocessor-cond-mode
  "Toggle mode line display of current c preprocessor conditional"
  :global t :group 'which-c-preprocessor-cond)

(provide 'which-c-preprocessor-cond)
