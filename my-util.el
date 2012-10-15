(require 'cl)

(defun fill-out-to-column (&optional width fill-char)
  "Insert FILL-CHAR at the end of the current line until the line
  is WIDTH columns wide. WIDTH defaults to 80 and FILL-CHAR
  defaults to a space (i.e. ?\s)"
  (interactive)
  (end-of-line)
  ;; some defaults
  (if (not width) (setq width 80))
  (if (not fill-char) (setq fill-char ?\s))
  (let ((n (- width (current-column))))
    (if (> n 0)
        (insert-char fill-char n))))


(defun get-buffer-by-regexp (pattern)
  "Returns the first buffer that matches PATTERN"
  (find pattern (buffer-list) :key 'buffer-name :test 'string-match-p))

;; function call face
(defun my-match-function-call (&optional limit)
  (message "Trying the matcher function fun from %d to %d" (point) limit)
  (while (and (search-forward-regexp "\\(\\w+\\)\\s-*\(" limit 'no-error)
              (not (save-match-data
                     (string-match c-keywords-regexp (match-string 1))))
              (not (save-excursion
                     (backward-char)
                     (forward-sexp)
                     (c-skip-ws-forward)
                     (or (eobp) (= ?\{ (char-after (point)))))))))

(defvar font-lock-function-call-face		'font-lock-function-call-face
  "Face name to use for function calls.")

;; could also use a regex:
;; ("\\(\\w+\\)\\s-*\(" (1 font-lock-function-call-face))
;; could use a match function:
;; (font-lock-add-keywords 'c++-mode
;;                         '((my-match-function-call . font-lock-function-call-face)))
;; could use a regexp:
(font-lock-add-keywords 'c++-mode
                        '(("\\(\\w+\\)\\s-*\(" (1 font-lock-function-call-face))))


;; one-line scrollers:
(defun my-up-a-line ()
  (interactive)
  (previous-line)
  (recenter))

(defun my-down-a-line ()
  (interactive)
  (next-line)
  (recenter))

(defun my-up-5-lines ()
  (interactive)
  (previous-line 5))

(defun my-down-5-lines ()
  (interactive)
  (next-line 5))

(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

;; cycle backwards
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

;; grep whatever i'm on by passing a prefix:
(defun grep-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'grep))

;; increment number at point:
(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
	(setq inc-by (if arg arg 1))
	(skip-chars-backward "0123456789")
	(when (re-search-forward "[0-9]+" nil t)
	  (setq field-width (- (match-end 0) (match-beginning 0)))
	  (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
	  (when (< answer 0)
	    (setq answer (+ (expt 10 field-width) answer)))
	  (replace-match (format (concat "%0" (int-to-string field-width) "d")
				 answer)))))))

(defun my-decrement-number-decimal (&optional arg)
  "Decrement"
  (interactive "p*")
  (if arg
      (my-increment-number-decimal (- 0 arg)) ;true
    (my-increment-number-decimal -1) ;else
    ))


(defun my-compile-unit-tests (test-loc what-to-do)
  "compile unit tests"
  (interactive)
  (message (concat "Compiling unit tests at " test-loc))
  (compile (concat "cd " test-loc " && " what-to-do)))


(defun my-run-unit-tests ()
  "Run specific unit tests"
  (interactive)
  (message "Getting available unit tests...")
  (let* ((the-test-loc (file-name-as-directory my-test-loc))
         (the-listing-command-output
          (shell-command-to-string (concat "schroot -p -- " the-test-loc "build/unittest -l")))
         (the-choices
          (cons "All Tests" (cdr (split-string the-listing-command-output "\n"))))
         (whichtests
          ;; join test names with a space
          (mapconcat 'identity
                     (ido-completing-read-multiple "Unit test cases to run: "
                                                   the-choices nil t)
                     " "))
         (the-command-args
          (if (string= whichtests "All Tests")
              ""
            whichtests))
         (the-command
          (concat "schroot -p -- " the-test-loc "build/unittest " the-command-args)))
    (message (concat "Running unittest" the-command-args))
    (async-shell-command the-command "*Unit Tests*")))



;; macro saving fun:
(defun save-macro (name)                  
  "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
  (interactive "SName of the macro: ")  ; ask for the name of the macro    
  (kmacro-name-last-macro name)         ; use this name for the macro    
  (find-file "~/.emacs.d/my-kmacros.el")                   ; open ~/.emacs or other user init file 
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro 
  (newline)                             ; insert a newline
  (save-buffer)                         ; save
  (switch-to-buffer nil))               ; return to the initial buffer


(defun ido-completing-read-multiple (prompt choices &optional predicate require-match initial-input hist def sentinel)
  "Read multiple items with ido-completing-read. Reading stops
  when the user enters SENTINEL. By default, SENTINEL is
  \"*done*\". SENTINEL is disambiguated with clashing completions
  by appending _ to SENTINEL until it becomes unique. So if there
  are multiple values that look like SENTINEL, the one with the
  most _ at the end is the actual sentinel value. See
  documentation for `ido-completing-read' for details on the
  other parameters."
  (let
      ((sentinel (if sentinel sentinel "*done*"))
       (done-reading nil)
       (res ()))

    ;; uniquify the SENTINEL value
    (while (find sentinel choices)
      (setq sentinel (concat sentinel "_")))
    (setq choices (cons sentinel choices))

    ;; read some choices
    (while (not done-reading)
      (setq this-choice (ido-completing-read prompt choices predicate require-match initial-input hist def))
      (if (equal this-choice sentinel)
          (setq done-reading t)
        (setq res (cons this-choice res))))

    ;; return the result
    res
    ))

(defun my-is-elf-executable-p (filename)
  "Returns t if the file is an elf executable"
  (string-match ".*ELF \\(32\\)*\\(64\\)*-bit LSB executable.*"
                (shell-command-to-string (concat "file " filename))))

(defun revert-and-goto-end-of-buffer ()
  "revert buffer and go to end of buffer"
  (interactive)
  (revert-buffer)
  (end-of-buffer))

(defun rebuild-the-unit-tests ()
  (interactive)
  (my-compile-unit-tests my-test-loc "schroot -p -- ./rebuild.sh"))

(defun build-the-unit-tests ()
  (interactive)
  (my-compile-unit-tests my-test-loc "schroot -p -- ./build.sh"))

(defun browse-url-chrome (url &rest args)
  "Use chrome to browse urls"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-browser-function 'browse-url-generic)
        (browse-url-generic-program "google-chrome")
        (browse-url-generic-args '("--enable-user-stylesheet")))
    (apply #'browse-url url args)))

(defun browse-url-chromium (url &rest args)
  "Use chromium to browse urls"
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-browser-function 'browse-url-generic)
        (browse-url-generic-program "chromium"))
    (apply #'browse-url url args)))

(defun my-lookup-current-word ()
  "look up word in dictionary"
  (interactive)
  (let ((tmpbufname "*WORD LOOKUP*")
        (shellcmd (concat "dict " (read-from-minibuffer "Look up word: " (current-word)))))
    (with-output-to-temp-buffer tmpbufname
      (shell-command shellcmd tmpbufname))))

(defun my-toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (if (= tab-width 8) 4 8))
  (redraw-display))

(defun my-search-all-buffers (regexp)
  "From http://www.emacswiki.org/emacs/SearchBuffers#toc9"
  (interactive "sRegexp: ")
  (multi-occur-in-matching-buffers "." regexp t))

(defun gtags-current-lineno ()
  "Get current line number"
  (if (= 0 (count-lines (point-min) (point-max)))
      0
    (save-excursion
      (end-of-line)
      (if (equal (point-min) (point))
          1
        (count-lines (point-min) (point))))))

(defun kill-where-i-am (&optional kill-full-path)
  "Put filename:linum in the kill ring. With prefix, include full
path to file."
  (interactive "P")
  (let ((x-select-enable-clipboard t)
	(the-path (if kill-full-path
		      buffer-file-name
		    (file-name-nondirectory (buffer-file-name)))))
    (kill-new (concat the-path
		      ":"
		      (number-to-string (gtags-current-lineno))))))

(defun grep-what-im-on ()
  "grep whatever i'm on by passing a prefix:"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'grep))

(defun my-make-this-buffer-writable ()
  (interactive)
  (let* ((currmodes (file-modes (buffer-name)))
         (newmodes (file-modes-symbolic-to-number "a+w")))
    (chmod (buffer-file-name) newmodes))
  (toggle-read-only)
  (message "made writable!"))

(defun ignore-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun my-previous-window ()
  (interactive)
  (setq current-prefix-arg '(-1))
  (call-interactively 'other-window))

(defun my-thousands-separate (num)
  "Formats the (possibly floating point) number with a thousands
separator."
  (let* ((nstr (number-to-string num))
         (dot-ind (string-match "\\." nstr))
         (nstr-no-decimal (if dot-ind
                               (substring nstr 0 dot-ind)
                             nstr))
         (nrest (if dot-ind
                    (substring nstr dot-ind)
                  nil))
         (pretty nil)
         (cnt 0))
    (dolist (c (reverse (append nstr-no-decimal nil)))
      (if (and (zerop (% cnt 3)) (> cnt 0))
          (setq pretty (cons ?, pretty)))
      (setq pretty (cons c pretty))
      (setq cnt (1+ cnt)))
    (concat pretty nrest)))

(defun my-prettify-number (n)
  "Prints a number to the minibuffer in a few delicious
formats. If `current-word' is a number, that's what is used,
otherwise we prompt the user."
  (interactive
   (let ((default
           (save-excursion
             (skip-chars-backward "0-9")
             (if (looking-at "[-+]?\\([0-9]*\.\\)?[0-9]+")
                 (string-to-number (current-word))
               (read-number "Number: ")))))
     (list default)))
  (let ((nstr (number-to-string n))
        (npretty (my-thousands-separate n)))
    (message "%s | %g | %d | %s" npretty n n nstr)))

(defun my-kill-last-message ()
  "Puts the last line from the *Messages* buffer onto the kill
ring"
  (interactive)
  (with-current-buffer "*Messages*"
    (goto-char (point-max))
    (forward-line -1)
    (clipboard-kill-ring-save (point) (line-end-position))
    (message "Killed last message: %s" (buffer-substring-no-properties
                                        (point)
                                        (line-end-position)))))

(defun my-make-buffer-a-scratch-buffer (buf loc)
  "Copy the contents of BUF into a temporary buffer, switch to
that buffer, and move point to LOC in the new buffer."
  (let ((new-buf (generate-new-buffer
                  (format"SCRATCH_%s" (buffer-name buf)))))
    (switch-to-buffer new-buf)
    (insert-buffer buf)
    (goto-char loc)))

(defun my-make-this-a-scratch-buffer ()
  (interactive)
  (my-make-buffer-a-scratch-buffer (current-buffer) (point)))

(defun convert-size-to-bytes (s)
  "Given a size with suffix K or M, returns the size in bytes"
  (let* ((slen (length s))
         (all-but-last (substring s 0 (- slen 1 )))
         (last-char (downcase (substring s (- slen 1) slen))))
    (cond
     ((string= last-char "k") (* 1024 (string-to-number all-but-last)))
     ((string= last-char "m") (* 1048576 (string-to-number all-but-last))))))

(defun get-rfc-list-downloaded-rfcs ()
  "Lists RFCs under `get-rfc-local-rfc-directory' along with
their titles. Requires rfc-index.txt to be in place."
  (interactive)
  (require 'get-rfc)
  (let ((rfcs (remove-if (lambda (f)
                           (or (file-directory-p f)
                               (string= "rfc-index.txt" f))) 
                         (directory-files get-rfc-local-rfc-directory)))
        (rfc-output '()))
    (with-temp-buffer
      (insert-file-contents (concat get-rfc-local-rfc-directory "/" get-rfc-rfc-index))
      (dolist (rfc rfcs)
        (goto-char 0)
        (let* ((rfc-match (string-match "[[:alpha:]]+\\([[:digit:]]+\\)\\.txt" rfc))
               (rfc-num (string-to-number (match-string 1
                                                        rfc)))
               (rfc-num-padded (format "%.4d" rfc-num))
               (beginning-of-title (re-search-forward (concat "^" rfc-num-padded " ")))
               (end-of-title (line-end-position))
               (the-title (buffer-substring beginning-of-title end-of-title)))
          (setq rfc-output
                  (cons (format "%6s %13s %s %s" rfc-num rfc " => " the-title)
                        rfc-output)))))
    (with-output-to-temp-buffer "*Downloaded RFCs*"
      (dolist (output-line rfc-output)
        (princ (concat output-line "\n"))))))

(defcustom pageview-narrow-to-page t
  "When non-nil, narrows to page whenever
  `pageview-goto-next-page-break' or
  `pageview-goto-previous-page-break' are used")

(defun pageview-toggle-narrow-to-page ()
  "toggles the value of `pageview-narrow-to-page'"
  (interactive)
  (setq pageview-narrow-to-page (not pageview-narrow-to-page)))

(defun pageview-navigate-page-break (how)
  "Navigates to the next or previous page break (depending on
  HOW) and recenters the screen with the page break at the top.

HOW should be `forward-page', `backward-page', or similar."
  (interactive)
  (widen)
  ;; (if (and pageview-narrow-to-page
  ;;          ;; extra backward-page when we're narrowing
  ;;          (equal how 'backward-page))
  ;;     (backward-page))
  (funcall how)
  (recenter 3)
  (when pageview-narrow-to-page
    (narrow-to-page)))

(defun pageview-goto-next-page-break ()
  "Navigates to the next page break and recenters the screen with
  the page break at the top."
  (interactive)
  (pageview-navigate-page-break 'forward-page))

(defun pageview-goto-previous-page-break ()
  "Navigates to the previous page break and recenters the screen
  with the page break at the top."
  (interactive)
  (pageview-navigate-page-break 'backward-page))

(defun my-put-prefix-on-paths (prefix paths)
  "Returns a list of paths with PREFIX prefixed onto each item"
  (mapcar (lambda (el)
            (concat (file-name-as-directory prefix)
                    (substring el 1)))
          paths))


(defun directory-dirs (dir &optional excludes)
  "Find all directories in DIR.

Adapted from http://emacswiki.org/emacs/ElispCookbook"
  (unless (file-directory-p dir)
    (error "Not a directory `%s'" dir))
  (let ((dir (directory-file-name dir))
        (dirs '())
        (files (directory-files dir nil nil t))
        (the-excludes (append '("." "..") excludes)))
    (dolist (file files)
      (unless (member file the-excludes)
        (let ((file (concat dir "/" file)))
          (when (file-directory-p file)
            (setq dirs (append (cons file
                                     (directory-dirs file excludes))
                               dirs))))))
    dirs))

(defun strip-leading-dir-from-each (dirs dir-to-strip)
  "Strips leading DIR-TO-STRIP from DIRS"
  (mapcar (lambda (el)
            (file-name-as-directory (replace-regexp-in-string dir-to-strip "/" el)))
          dirs))

(defun my-transpose-windows (arg)
   "Transpose the buffers shown in two windows. From
http://emacswiki.org/emacs/TransposeWindows"
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(defun my-go-to-corresponding-header-or-implementation-file ()
  "Goes to the .c or .h that corresponds to the current file"
  (interactive)
  (let* ((file-base-name (file-name-sans-extension (buffer-file-name)))
	 (file-extension (file-name-extension (buffer-file-name)))
	 (the-other-file (concat file-base-name
				 (cond
				  ((string= file-extension "c") ".h")
				  ((string= file-extension "cpp") ".h")
				  ((and (string= file-extension "h")
					(file-exists-p (concat file-base-name ".c"))) ".c")
				  ((and (string= file-extension "h")
					(file-exists-p (concat file-base-name ".cpp"))) ".cpp")
				  (t nil)))))
    (if (not (or (string= file-extension "c")
		 (string= file-extension "cpp")
		 (string= file-extension "h")))
	(message "%s.%s is not a .c, .cpp, or .h file." (file-name-nondirectory file-base-name) file-extension)
      (if (or (not the-other-file) (not (file-exists-p the-other-file)))
	  (message "%s does not exist" the-other-file)
	(find-file the-other-file)))))

;; (defun my-gnus-get-group-names ()
;;   "Returns a list of currently subscribed gnus group names"
;;   (let (choices group)
;;     (mapatoms (lambda (symbol)
;; 		(setq group (symbol-name symbol))
;; 		(push (if (string-match "[^\000-\177]" group)
;; 			  (gnus-group-decoded-name group)
;; 			group)
;; 		      choices))
;; 	      gnus-active-hashtb)
;;     choices))

(defun my-gnus-get-group-names ()
  "get gnus group names"
  (with-current-buffer gnus-group-buffer
    (save-excursion
      (gnus-group-list-all-groups)
      (goto-char (point-min))
      (let ((group-names nil)
	    (more-lines t))
	(while more-lines
	  (push (gnus-group-name-at-point) group-names)
	  (setq more-lines (= 0 (forward-line 1))))
	(cdr group-names)))))

(defun my-gnus-subscribed-to-group-p (group)
  "Checks that we are subscribed to `group'"
  (member group (my-gnus-get-group-names)))

(defun my-gnus-verify-subscriptions (groups)
  "Verify that we are subscribed to all groups in
  `groups'. Prompt to subscribe for any unsubscribed group."
  (interactive)
  (let ((group-names (my-gnus-get-group-names)))
    (dolist (group groups)
      (unless (member group group-names)
	(message "Not subscribed to %s. You might want to add a subscription..." group)))))

(defun my-gnus-verify-subscriptions-hook ()
  "Just calls `my-gnus-verify-subscriptions' with the first item
in each `nnmail-split-methods'"
  (interactive)
  (my-gnus-verify-subscriptions (mapcar 'first nnmail-split-methods)))

(defun my-make-room-for-new-compilation-buffer ()
  "Renames existing *compilation* buffer to something unique so
  that a new compilation job can be run."
  (interactive)
  (let ((cbuf (get-buffer "*compilation*"))
	(more-cbufs t)
	(n 1)
	(new-cbuf-name ""))
    (when cbuf
      (while more-cbufs
	(setq new-cbuf-name (format "*compilation%d*" n))
	(setq n (1+ n))
	(setq more-cbufs (get-buffer new-cbuf-name)))
      (with-current-buffer cbuf
	(rename-buffer new-cbuf-name)))))


(defvar my-compilers-alist nil
  "alist to be used for compilation. Each element in the list
  should be a cons cell of the form:

(compiler-name . compile-fn)")

(setq workspace-dir "/local/mnt/workspace/mitchelh")
(setq jb-dir (concat workspace-dir "/jb"))
(setq kdev-dir (concat workspace-dir "/kdev"))
(setq mdm-dir (concat workspace-dir "/mdm"))

(setq my-compilers-alist
      `(("android msm8960 jb" . ,(my-make-android-compiler jb-dir
							   "1 msm8960 3"))
	("android msm8660_surf jb" . ,(my-make-android-compiler jb-dir
								"1 msm8660_surf 3"))
	("android msm7627a jb" . ,(my-make-android-compiler jb-dir
								"1 msm7627a 3"))
	("android msm7630_surf jb" . ,(my-make-android-compiler jb-dir
								"1 msm7630_surf 3"))
	("android msm8974 jb" . ,(my-make-android-compiler jb-dir
								"1 msm8974 3"))
	("kdev msm8960 buildroot" . ,(my-make-kdev-compiler "msm8960_defconfig" kdev-dir))
	("kdev msm8974 buildroot" . ,(my-make-kdev-compiler "msm8974_defconfig" kdev-dir))
	("kdev msm9625 buildroot" . ,(my-make-kdev-compiler "msm9625_defconfig" kdev-dir))
	("kdev msm8226 buildroot" . ,(my-make-kdev-compiler "msm8974_defconfig" kdev-dir))
	("gtags kernel jb" . ,(my-make-gtags-compiler (concat jb-dir "/kernel")))
	("gtags kernel-tests" . ,(my-make-gtags-compiler (concat kdev-dir "/kernel-tests")))
	("gtags mdm" . ,(my-make-gtags-compiler (concat mdm-dir "/kernel")))))


(defun my-choose-and-use-compiler ()
  "choose a compiler from `my-compilers-alist' using
`ido-completing-read'"
  (interactive)
  (let* ((the-choices (mapcar 'car my-compilers-alist))
	 (the-choice (ido-completing-read "Compiler to run: "
					  the-choices))
	 (the-compile-fn (cdr (assoc the-choice
	 			     my-compilers-alist))))
    (funcall the-compile-fn)))
