(defmacro my-make-recentering-advice (after-what)
  "Macro to define advice to `recenter' after AFTER-WHAT"
  (let ((advsymbol (intern (concat "make-recenter-" (symbol-name after-what))))
	(after-what-str after-what))
    `(defadvice ,after-what (after advsymbol
				    activate)
       "Recenter the page after doing this thing"
       ;; (message (concat "Running advice for " (symbol-name ',after-what-str)))
       (recenter))))

(defmacro my-make-android-compiler (droid-root choosecombo-args)
  "make android compiler (for use in `my-compilers')"
  `(lambda ()
     (interactive)
     (let ((default-directory ,droid-root)
	   (compilation-search-path ,droid-root)
	   (compile-command (format "d=%s; cd $d; source build/envsetup.sh; choosecombo %s; make -j8 -C $d"
				    ,droid-root ,choosecombo-args)))
       (cd default-directory)
       (call-interactively 'compile))))

(defmacro my-make-gtags-compiler (where)
  "make gtags compiler (for use in `my-compilers'). Runs gtags
-iI at `where'"
  `(lambda ()
     (interactive)
     (let ((compile-command (format "cd %s; gtags -iI && notify-send \"gtags regeneration SUCCESSFUL\" || notify-send \"gtags regeneration FAILED\""
				    ,where)))
       (call-interactively 'compile))))

(defmacro my-make-kdev-compiler (where)
  "make kdev compiler (for use in `my-compilers')."
  `(lambda ()
     (interactive)
     (let ((compile-command (format "make -C %s && notify-send \"kdev compile SUCCESSFUL\" || notify-send \"kdev compile FAILED\""
				    ,where)))
       (call-interactively 'compile))))
