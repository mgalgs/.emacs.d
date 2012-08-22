(defmacro my-make-recentering-advice (around-what)
  (let ((advsymbol (intern (concat "make-recenter-" (symbol-name around-what))))
	(around-what-str around-what))
    `(defadvice ,around-what (after advsymbol
				    activate)
       "Recenter the page after doing this thing"
       ;; (message (concat "Running advice for " (symbol-name ',around-what-str)))
       (recenter))))
