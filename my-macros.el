(defmacro my-make-recentering-advice (after-what)
  "Macro to define advice to `recenter' after AFTER-WHAT"
  (let ((advsymbol (intern (concat "make-recenter-" (symbol-name after-what))))
	(after-what-str after-what))
    `(defadvice ,after-what (after advsymbol
				    activate)
       "Recenter the page after doing this thing"
       ;; (message (concat "Running advice for " (symbol-name ',after-what-str)))
       (recenter))))

(defun my-make-notify-send-depending-on-retval (what)
  "to be concatenated to a shell command"
  (format "&& { notify-send \"%s SUCCESSFUL\"; beep -f 3000 -l 50 -r 3; } || { notify-send \"%s FAILED\"; beep -f 200 -l 200 -r 2; }"
	  what what))

(defmacro my-make-android-compiler (droid-root choosecombo-args)
  "make android compiler (for use in `my-compilers')"
  `(lambda ()
     (interactive)
     (let ((default-directory ,droid-root)
	   (compilation-search-path ,droid-root))
       (setq compile-command (format "d=%s; cd $d; source build/envsetup.sh; choosecombo %s; make -j8 -C $d %s"
				     ,droid-root ,choosecombo-args (my-make-notify-send-depending-on-retval "android build")))
       (cd default-directory)
       (call-interactively 'compile))))

(defmacro my-make-gtags-compiler (where)
  "make gtags compiler (for use in `my-compilers'). Runs gtags
-iI at `where'"
  `(lambda ()
     (interactive)
     (setq compile-command (format "cd %s; gtags -iI %s"
				   ,where (my-make-notify-send-depending-on-retval "gtags regeneration")))
     (call-interactively 'compile)))

(defmacro my-make-kdev-compiler (kconfig where)
  "make kdev compiler (for use in `my-compilers')."
  `(lambda ()
     (interactive)
     (setq compile-command (format "KCONFIG=%s make -C %s %s"
				   ,kconfig ,where (my-make-notify-send-depending-on-retval "kdev compile")))
     (call-interactively 'compile)))
