(defmacro my-make-recentering-advice (after-what)
  "Macro to define advice to `recenter' after AFTER-WHAT"
  (let ((advsymbol (intern (concat "make-recenter-" (symbol-name after-what))))
	(after-what-str after-what))
    `(defadvice ,after-what (after advsymbol
				    activate)
       "Recenter the page after doing this thing"
       ;; (message (concat "Running advice for " (symbol-name ',after-what-str)))
       (recenter))))
