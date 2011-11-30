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
          (shell-command-to-string (concat the-test-loc "build/unittest -l")))
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
          (concat the-test-loc "build/unittest " the-command-args)))
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
  (my-compile-unit-tests my-test-loc "./rebuild.sh"))

(defun build-the-unit-tests ()
  (interactive)
  (my-compile-unit-tests my-test-loc "./build.sh"))

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

(defun kill-where-i-am ()
  "put filename:linum in the kill ring"
  (interactive)
  (kill-new
   (concat (file-name-nondirectory (buffer-file-name))
	   ":"
	   (number-to-string (gtags-current-lineno)))))

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
