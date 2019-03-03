(require 'cl)
(require 'thingatpt)

(defun directories-in-directory (directory)
  "Returns a list of all directories contained in DIRECTORY"
  (let* ((all-files (mapcar (lambda (el)
                              (concat (file-name-as-directory directory) el))
                            (directory-files directory)))
         ;; remove '.', '..' and all non-directories
         (filtered-files (remove-if (lambda (el)
                                      (or (string= "." (substring el -1))
                                          (not (file-directory-p el))))
                                    all-files)))
    filtered-files))

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
(defun m/match-function-call (&optional limit)
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
;;                         '((m/match-function-call . font-lock-function-call-face)))
;; could use a regexp:
(font-lock-add-keywords 'c++-mode
                        '(("\\(\\w+\\)\\s-*\(" (1 font-lock-function-call-face))))


;; one-line scrollers:
(defun m/up-a-line ()
  (interactive)
  (previous-line)
  (recenter))

(defun m/down-a-line ()
  (interactive)
  (next-line)
  (recenter))

(defun m/up-5-lines ()
  (interactive)
  (previous-line 5))

(defun m/down-5-lines ()
  (interactive)
  (next-line 5))

(defun m/horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))

;; cycle backwards
(defun m/yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

;; grep whatever i'm on by passing a prefix:
(defun grep-with-prefix-arg ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'grep))

;; increment number at point:
(defun m/increment-number-decimal (&optional arg)
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

(defun m/decrement-number-decimal (&optional arg)
  "Decrement"
  (interactive "p*")
  (if arg
      (m/increment-number-decimal (- 0 arg)) ;true
    (m/increment-number-decimal -1) ;else
    ))


(defun m/compile-unit-tests (test-loc what-to-do)
  "compile unit tests"
  (interactive)
  (message (concat "Compiling unit tests at " test-loc))
  (compile (concat "cd " test-loc " && " what-to-do)))


(defun m/run-unit-tests ()
  "Run specific unit tests"
  (interactive)
  (message "Getting available unit tests...")
  (let* ((the-test-loc (file-name-as-directory m/test-loc))
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
  (find-file "~/.emacs.d/m/kmacros.el")                   ; open ~/.emacs or other user init file 
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

(defun m/is-elf-executable-p (filename)
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
  (m/compile-unit-tests m/test-loc "schroot -p -- ./rebuild.sh"))

(defun build-the-unit-tests ()
  (interactive)
  (m/compile-unit-tests m/test-loc "schroot -p -- ./build.sh"))

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

(defun m/lookup-current-word ()
  "look up word in dictionary"
  (interactive)
  (let ((tmpbufname "*WORD LOOKUP*")
        (shellcmd (concat "dict " (read-from-minibuffer "Look up word: " (current-word)))))
    (with-output-to-temp-buffer tmpbufname
      (shell-command shellcmd tmpbufname))))

(defun m/toggle-tab-width-setting ()
  "Toggle setting tab widths between 4 and 8"
  (interactive)
  (setq tab-width (if (= tab-width 8) 4 8))
  (redraw-display))

(defun m/search-all-buffers (regexp)
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

(defun mgalgs--get-where-i-am (include-line-number)
  (let ((the-path buffer-file-name)
        (suffix (if include-line-number
                    (concat ":"
                            (number-to-string (line-number-at-pos)))
                  "")))
    (concat the-path suffix)))

(defun m/kill-where-i-am (&optional include-line-number)
  "Put filename in the kill ring. With prefix, include the line
number."
  (interactive "P")
  (let ((x-select-enable-clipboard t))
    (kill-new (mgalgs--get-where-i-am include-line-number))))

;; for string-remove-prefix and friends
;; http://ergoemacs.org/emacs/elisp_trim_string.html
(require 'subr-x)

(defun m/gitroot ()
  (concat (string-trim-right (shell-command-to-string "git rev-parse --show-toplevel"))
          "/"))

(defun m/kill-where-i-am-relative-to-gitroot (include-line-number)
  "Like `m/kill-where-i-am' but is relative to the gitroot
rather than the absolute path"
  (interactive "P")
  (let ((x-select-enable-clipboard t)
        (gitroot (m/gitroot)))
    (kill-new (string-remove-prefix gitroot
                                    (mgalgs--get-where-i-am include-line-number)))))

(defun m/grep-what-im-on ()
  "grep whatever i'm on by passing a prefix:"
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'grep))

(defun m/make-this-buffer-writable ()
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

(defun m/previous-window ()
  (interactive)
  (setq current-prefix-arg '(-1))
  (call-interactively 'other-window))

(defun m/thousands-separate (num)
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

(defun m/prettify-number (n)
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
        (npretty (m/thousands-separate n)))
    (message "%s | %g | %d | %s" npretty n n nstr)))

(defun m/kill-last-message ()
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

(defun m/make-buffer-a-scratch-buffer (buf loc)
  "Copy the contents of BUF into a temporary buffer, switch to
that buffer, and move point to LOC in the new buffer."
  (let ((new-buf (generate-new-buffer
                  (format"SCRATCH_%s" (buffer-name buf)))))
    (switch-to-buffer new-buf)
    (insert-buffer buf)
    (goto-char loc)))

(defun m/make-this-a-scratch-buffer ()
  (interactive)
  (m/make-buffer-a-scratch-buffer (current-buffer) (point)))

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

(defun m/put-prefix-on-paths (prefix paths)
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

(defun m/transpose-windows (arg)
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

(defun m/go-to-corresponding-header-or-implementation-file ()
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

;; (defun m/gnus-get-group-names ()
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

(defun m/gnus-get-group-names ()
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

(defun m/gnus-subscribed-to-group-p (group)
  "Checks that we are subscribed to `group'"
  (member group (m/gnus-get-group-names)))

(defun m/gnus-verify-subscriptions (groups)
  "Verify that we are subscribed to all groups in
  `groups'. Prompt to subscribe for any unsubscribed group."
  (interactive)
  (let ((group-names (m/gnus-get-group-names)))
    (dolist (group groups)
      (unless (member group group-names)
	(message "Not subscribed to %s. You might want to add a subscription..." group)))))

(defun m/gnus-verify-subscriptions-hook ()
  "Just calls `m/gnus-verify-subscriptions' with the first item
in each `nnmail-split-methods'"
  (interactive)
  (m/gnus-verify-subscriptions (mapcar 'first nnmail-split-methods)))

(defun m/once ()
  "Insert header guards"
  (interactive)
  (let ((guard-txt (format "__%s__"
			   (replace-regexp-in-string (regexp-quote ".")
						     "_"
						     (upcase (buffer-name))))))
    (insert (format "#ifndef %s\n#define %s\n\n#endif /* %s */\n" guard-txt guard-txt guard-txt))
    (previous-line 2)
    (beginning-of-line)))

(defun m/xdg-open-each-in-region ()
  "Run xdg-open on each line in current region"
  (interactive)
  (shell-command-on-region (region-beginning)
			   (region-end)
			   "while read m; do xdg-open $m; done"))

(defun m/make-shell-caller (proggie)
  "Function to define a function that does a shell-command to
  program `proggie' with a single argument: the current word."
  (defalias (intern (concat "m/shell-" proggie))
    `(lambda (&optional in)
       ,(format "Run %s on region (if active) or (current-word) or the supplied argument." proggie)
       (interactive)
       (let* ((the-word (or in
                            (if (region-active-p)
                                (buffer-substring (point)
                                                  (mark))
                              (current-word))))
              (cmd (format "%s %s" ,proggie the-word))
              (output (shell-command-to-string cmd))
              (output-clean (car (split-string output "\n"))))
         output-clean))))

(defun m/isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun m/isearch-yank-word-hook ()
  (when (equal this-command 'm/isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      (isearch-search-and-update))))

(add-hook 'isearch-mode-hook 'm/isearch-yank-word-hook)

(defun m/bit-numberer (numstring &optional arg)
  "Number the bits in `NUMSTRING'.

NUMSTRING is a string representing a binary or hex number.

When give a prefix arg, also prompts for a minimum bit width."
  (interactive "sBinary or hex number: \nP")
  (let (output-list output-string bitwidth)
    (when (string-prefix-p "0x" numstring)
      (setq numstring (hex2bin numstring)))
    (if arg
        (setq bitwidth (read-number "Bit width: " 32))
      (setq bitwidth 32))
    (setq numstring (s-pad-left bitwidth "0" numstring))
    (loop for i downfrom (1- (length numstring)) to 0
	  do
	  (setq output-list (cons (format "|%-3d" i) output-list)))
    (setq output-list (cons "\n" output-list))
    (dolist (ch (mapcar 'identity numstring))
      (setq output-list (cons (format "|%-3c" ch) output-list)))
    (setq output-string (mapconcat 'identity
				   (reverse output-list)
				   ""))
    (message output-string)))

(defun m/occur-region-or-symbol-at-point (&optional nlines the-point the-mark)
  "Run `occur' with `symbol-at-point' or the region, if active."
  (interactive "P\nr")
  (occur (substring-no-properties (if (use-region-p)
                                      (buffer-substring-no-properties the-point
                                                                      the-mark)
                                    (thing-at-point 'symbol)))
         (if (numberp nlines)
             nlines
           0)))

(defun m/show-commit-at-point ()
  "Run `magit-show-commit` with `word-at-point`."
  (interactive)
  (magit-show-commit (word-at-point)))

(defun m/switch-to-buffer-and-shrink (buf)
  "Switch to buffer and call `shrink-window-if-larger-than-buffer'"
  (interactive "b")
  (shrink-window-if-larger-than-buffer (get-buffer-window buf)))

(defun m/rescan-and-add-to-load-path ()
  "Re-scans ~/.emacs.d/site-lisp and adds missing directories to
load-path"
  (interactive)
  (let ((new-dirs (-difference (directories-in-directory "~/.emacs.d/site-lisp")
                               load-path)))
    (message "Adding to load-path: %s" (if new-dirs
                                           new-dirs
                                         "NOTHING! All sync'd up."))
    (setq load-path (-distinct (append load-path
                                       new-dirs)))))

;; Credit:
;; https://github.com/magnars/.emacs.d/blob/master/defuns/lisp-defuns.el
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun m/get-buffer-point-function (el)
  "Get the name of the function at point in buffer. el should be
a cons cell of the form (buffer . point)"
  (let ((buffer (car el))
        (the-point (cdr el)))
    (with-current-buffer buffer
      (cond
       ((equal major-mode 'c-mode)
        (goto-char the-point)
        (c-defun-name))
       ((equal major-mode 'asm-mode)
        (goto-char the-point)
        (buffer-substring-no-properties the-point
                                        (save-excursion
                                          (end-of-line)
                                          (point))))))))

(defun m/gtags-show-current-gtags-call-flow ()
  "Displays gtags call flow"
  (interactive)
  (switch-to-buffer-other-window "*Call Flow*")
  (erase-buffer)
  (insert (mapconcat 'identity
                     (delete nil
                             (mapcar 'm/get-buffer-point-function
                                     (-zip gtags-buffer-stack
                                           gtags-point-stack)))
                     "\n"))
  (shrink-window-if-larger-than-buffer (get-buffer-window))
  (view-mode))

(defun m/gtags-pop-deleted-buffers ()
  "Clean up the gtags context stack by popping any deleted buffers"
  (interactive)
  (let ((cnt 1))
    (while (and gtags-buffer-stack
                (not (buffer-live-p (car gtags-buffer-stack))))
      (message "Popping killed buffer (%d)" cnt)
      (gtags-pop-context)
      (setq cnt (1+ cnt)))))

(defun m/gtags-pop-all-buffers ()
  "Clean up the entire gtags context stack"
  (interactive)
  (let ((cnt 1))
    (while gtags-buffer-stack
      (message "Popping buffer (%d) (%s)" cnt (car gtags-buffer-stack))
      (gtags-pop-context)
      (setq cnt (1+ cnt)))))

(defun m/add-footnote (footnote)
  "Add a footnote (read from minibuffer)"
  (interactive "sFootnote: ")
  (Footnote-add-footnote)
  (insert footnote)
  (Footnote-back-to-message))

(defun m/open-line-after-this-line ()
  "Like vim's `o' command"
  (interactive)
  (let ((next-line-add-newlines t))
    (next-line))
  (open-line 1))

(defun m/open-line-before-this-line ()
  "Like vim's `O' command"
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(defun m/compilation-start-hook-to-remember-last (process)
  (when (s-starts-with? "*compilation"
                        (buffer-name))
    (setq m/last-compilation-buffer (current-buffer))))

(defvar m/last-compilation-buffer nil)
(add-hook 'compilation-start-hook 'm/compilation-start-hook-to-remember-last)

(defun m/recompile ()
  "Switch to *compilation-XXX* and do a recompile"
  (interactive)
  (if m/last-compilation-buffer
    (-when-let (candidate-buffer (get-buffer m/last-compilation-buffer))
      (switch-to-buffer candidate-buffer)
      (recompile)
      (end-of-buffer))
    (message "No recent compilation. Run `M-x compile' first")))

(defun m/open-compilation-buffer ()
  (interactive)
  (-when-let (candidate (or m/last-compilation-buffer (get-buffer "*compilation*")))
    (switch-to-buffer candidate)))

(defun m/helm-completing-read-must-match (prompt choices &optional require-match
                                    initial-input history def inherit-input-method)
    "Wrapper for `helm-comp-read' that also sets :must-match to t"
    (helm-comp-read prompt
                    choices
                    :initial-input initial-input
                    :default def
                    :must-match t
                    :history history
                    ))

(defun strip-text-properties(txt)
  "Does what it sayd. http://stackoverflow.com/a/8377127/209050"
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun m/-extract-field-from-region (start end N delimiter)
  "Extract field `N' delimited by `delimiter' from region
specified by `start' and `end'"
  (goto-char start)
  (beginning-of-line)
  (let ((current-point (point))
        (result))
    (while (< current-point end)
      (push (nth N
                 (split-string (strip-text-properties (thing-at-point 'line))
                               delimiter))
            result)
      (forward-line)
      (setq current-point (point)))
    (reverse result)))

(defun m/mapcfield (fn N &optional delimiter)
  "Like `mapc' but evaluates `fn' for each field in the region defined by
`N' (field number) and `delimiter' (using `m/-extract-field-from-region')."
  (setq delimiter (or delimiter " "))
  (mapc fn (m/-extract-field-from-region (region-beginning)
                                          (region-end)
                                          N
                                          delimiter)))

(defun m/extract-field-from-region (start end)
  "Like cut -dD -fN where D and N are read from the user"
  (interactive "r")
  (let ((res (m/-extract-field-from-region start
                                            end
                                            (string-to-number (read-from-minibuffer "Field: "
                                                                                    "0"))
                                            (read-from-minibuffer "Field [default=\" \"]: "
                                                                  " "))))
    (message "%s" (mapconcat 'identity res "\n"))))

(defun m/view-and-switch-to-echo-area-messages ()
  (interactive)
  (view-echo-area-messages)
  (other-window 1)
  (end-of-buffer))

(defun m/indent-last-kill ()
  (interactive)
  (with-temp-buffer
    (yank)
    (let ((txt (replace-regexp-in-string "\t" "    " (buffer-string))))
      (erase-buffer)
      (insert (mapconcat (lambda (el) (concat "    " el))
                         (split-string txt "\n")
                         "\n")))
    (delete-trailing-whitespace)
    (kill-new (buffer-string))))

(defun m/hours-minutes-to-decimal (hours-minutes)
  "Convert `hours-mintes' (which should be a string like
\"1:45\") to a decimal representation (so \"1:45\" => 1.75)."
  (save-match-data
    (let* ((parts (split-string hours-minutes ":"))
           (hours (string-to-number (nth 0 parts)))
           (minutes (string-to-number (nth 1 parts))))
      (+ hours (/ minutes 60.0)))))

(defun m/expand-file-name-at-point ()
  "Use hippie-expand to expand the filename.

From http://superuser.com/a/68648/70130"
  (interactive)
  (let ((hippie-expand-try-functions-list '(try-complete-file-name-partially
                                            try-complete-file-name)))
    (call-interactively 'hippie-expand)))

(defun m/nth-match (st regexp n)
  "Returns the nth match of `regexp' on string `st', saving match data."
  (save-match-data
    (when (string-match regexp st)
      (match-string-no-properties n st))))

(defun m/ftrace-log-split ()
  "Split current buffer (an ftrace function_graph tracer log)
into columns based on the CPU number. This should really be more
generic (split into columns based on regex)."
  (interactive)
  (let* ((lines (split-string (buffer-string) "\n"))
         (max-line-len (apply 'max (mapcar 'length (-filter (lambda (el) (string-match "^ \\([[:digit:]]\\)" el))
                                                            lines)))))
    (message "max-line-len is %d" max-line-len)
    (with-current-buffer (get-buffer-create "*ftrace-log-split*")
      (erase-buffer)
      (dolist (line lines)
        (-if-let (cpu (m/nth-match line
                                    "^ \\([[:digit:]]\\)"
                                    1))
            (insert (format "%s%s"
                            (make-string (+ (* (string-to-number cpu) max-line-len)
                                            4)
                                         ?\ )
                            line))
          (insert line))
        (insert "\n"))))
  (switch-to-buffer "*ftrace-log-split*"))

(defun m/open-in-atom ()
  "Open the current buffer in the Atom editor"
  (interactive)
  (start-process "open-in-atom" nil "atom" "-f" (buffer-file-name)))

(defun m/checkpatch-current ()
  "Runs the kernel scripts/checkpatch.pl script on the current file"
  (interactive)
  (compile (concat "$(git rev-parse --show-toplevel)/scripts/checkpatch.pl --emacs --file "
                   (file-name-nondirectory (buffer-file-name)))))

;; adapted from http://pastie.org/9374830
(defun m/smooth-scroll (how top)
  (while (> top 10)
    (funcall how top)
    (sit-for (/ 1.0 (* top 3)))
    (setq top (/ top 2)))
  (dolist (n '(8 4 2 1))
    (sit-for (/ 1.0 (+ n 20)))
    (funcall how n)))

(defun m/smooth-scroll-down ()
  (interactive)
  (m/smooth-scroll 'scroll-up (/ (window-height) 2)))

(defun m/smooth-scroll-up ()
  (interactive)
  (m/smooth-scroll 'scroll-down (/ (window-height) 2)))

(defun m/gnus-patch-save-name (newsgroup headers &optional last-file)
  (let* ((subject-stripped (cadr (s-match "\\[PATCH.*\\] \\(.*\\)"
                                          (mail-header-subject headers))))
         (subject (replace-regexp-in-string "[^a-z]" "-" (downcase subject-stripped)))
         (from-email (cadr (s-match "<\\(.*\\)@" (mail-header-from headers)))))
    (concat from-email "-" subject ".patch")))

(defun m/article-get-message-url ()
  (interactive)
  (let ((url (concat "http://mid.gmane.org/" (mail-header-message-id gnus-current-headers))))
    (when (called-interactively-p)
      (message url))
    url))

(defun m/article-open-on-the-web ()
  "Opens the current article on the web"
  (interactive)
  (browse-url (m/article-get-message-url)))

(defun m/suggest-commit-message-prefix ()
  "Looks at recent commits for the currently staged files and
suggests some commit message prefixes."
  (interactive)
  (magit-with-toplevel
    (let* ((all-prefixes (mapcar (lambda (el) (car (s-match ".*: "
                                                            (substring el 1))))
                                 (magit-git-lines "log" "--no-merges" "--pretty=\"%s\"" "-100" "--"
                                                  (magit-git-lines "diff" "--cached" "--name-only"))))
           (uniq-prefixes (-uniq (-filter 'identity all-prefixes)))
           (counted-prefixes (mapcar (lambda (el) (cons el
                                                        (-count (lambda (el2) (string= el2 el))
                                                                all-prefixes)))
                                     uniq-prefixes))
           (sorted-choices (-sort (lambda (c1 c2) (> (cdr c1) (cdr c2)))
                                  counted-prefixes))
           (formatted-choices (mapcar (lambda (el) (format "%s (used %d time%s recently)"
                                                           (car el)
                                                           (cdr el)
                                                           (if (= (cdr el) 1)
                                                               ""
                                                             "s")))
                                      sorted-choices)))
      (when (> (length formatted-choices) 0)
        (insert (first (split-string (ido-completing-read "Commit message prefix: "
                                                          formatted-choices)
                                     " (used .* time.* recently)"))))
      formatted-choices)))

(defun m/underline-previous-line (&optional arg)
  "Adds an ascii underline to the previous line using `='
characters.  TODO: add support for different characters."
  (interactive "P")
  (let ((line-len (save-excursion
                    (forward-line -1)
                    (let ((col (current-column)))
                      (end-of-line)
                      (- (current-column) col))))
        (c (if arg (read-char "Underline char: ") ?=)))
    (insert (make-string line-len c) "\n")))

(defvar m--add-include-history nil)

(defun m/goto-includes ()
  "Goes to the top #include block"
  (interactive)
  (-if-let (includes (save-excursion
                       (goto-char 0)
                       (-when-let (includes (re-search-forward "^#include " nil t))
                         (beginning-of-line)
                         (point))))
      (progn
        (push-mark)
        (goto-char includes)
        (recenter))
    (message "Couldn't find any includes in this file!")))

(defun m/add-include (arg)
  "Adds a #include to the current buffer (presumably a C source
or header file), by appending it to the first block of existing
#include's.

With a prefix arg, just goto this file's includes."
  (interactive "P")
  (if arg
      (m/goto-includes)
    (let ((include (read-string "#include: "
                                nil
                                m--add-include-history)))
      (save-excursion
        (goto-char 0)
        (when (re-search-forward "^#include " nil t)
          (forward-paragraph)
          (insert "#include " include "\n"))))))

(defun m/shell-here ()
  "Create a new tmux window in the current directory and switch
to it."
  (interactive)
  (let ((tmux-cmd (format "tmux new-window -c \"%s\""
                          (expand-file-name default-directory)))
        (wmctrl-cmd (format "wmctrl -a 'zsh - \"%s@%s: '"
                            (user-login-name)
                            system-name)))
    (message "Trying %s" tmux-cmd)
    (shell-command tmux-cmd)
    (sit-for 1.5)
    (message "Trying %s" wmctrl-cmd)
    (shell-command wmctrl-cmd)))

;; Pull from PRIMARY (same as middle mouse click)
;;; from http://stackoverflow.com/a/28492272/209050
(defun m/get-primary ()
  (interactive)
  (insert
   (gui-get-primary-selection)))

(defun m/query-replace-using-region (start end)
  "Like `query-replace' but uses region as the search string"
  (interactive "r")
  (let ((use-region-p nil)
        (from (buffer-substring-no-properties start end)))
    (deactivate-mark)
    (goto-char start)
    (perform-replace from
                     (read-from-minibuffer (format "Replace %s with: "
                                                   from))
                     t nil nil)))

(provide 'my-util)
