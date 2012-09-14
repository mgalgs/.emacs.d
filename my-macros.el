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
	   (compile-command (format "env $(echo_the_exports=y; cd %s; source build/envsetup.sh; choosecombo %s; ) sh -c 'd=%s; cd $d; make -j8 -C $d'"
				    ,droid-root ,choosecombo-args ,droid-root)))
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
