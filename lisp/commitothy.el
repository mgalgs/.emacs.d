;;; commitothy.el --- Commit message generation via commitothy -*- lexical-binding: t; -*-

(defcustom commitothy-executable "~/src/commitothy/commitothy.py"
  "Path to the commitothy Python script."
  :type 'file
  :group 'commitothy)

(defcustom commitothy-model "qwen/qwen3-235b-a22b-2507"
  "LLM to use (passed to commitothy as `--model')."
  :type 'string
  :group 'commitothy)

(defcustom commitothy-options nil
  "Additional command line options to pass to `commitothy-executable'"
  :type '(repeat string)
  :group 'commitothy)

(defun commitothy--run (&rest args)
  "Run commitothy.py with ARGS and return its output string."
  (let* ((args (delq nil args))
         (command (mapconcat #'shell-quote-argument
                             (cons (expand-file-name commitothy-executable)
                                   (append (list "--model" commitothy-model)
                                           args
                                           commitothy-options))
                             " ")))
    (message "Running commitothy cmd: %s" command)
    (shell-command-to-string command)))

(defun commitothy--insert-output (output)
  "Insert OUTPUT at point and fill paragraphs."
  (let ((start (point)))
    (insert output)
    ;; We don't want to fill code review, if any. Search back for beginning of code
    ;; review block that signals the end of the commit message.
    (when (search-backward-regexp "^# \\*\\*\\* CODE REVIEW" nil t)
      (backward-paragraph))
    (let ((end (point)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (fill-paragraph nil)
          (forward-paragraph))))))

;;;###autoload
(defun commitothy-write-commit-message (arg)
  "Generate a new commit message via commitothy.
With prefix ARG, also pass --head to analyze HEAD instead of staged changes."
  (interactive "P")
  (commitothy--insert-output
   (commitothy--run (when arg "--head"))))

;;;###autoload
(defun commitothy-improve-commit-message (arg)
  "Improve the current COMMIT_EDITMSG buffer using commitothy.
Saves the buffer first, then calls commitothy with
--improve-message and passes point as cursor position.

With prefix ARG, also pass --head to analyze HEAD instead of staged changes."
  (interactive "P")
  (when git-commit-mode
    (save-buffer)
    (let* ((cursor-pos (number-to-string (1- (position-bytes (point)))))
           (args (list "--improve-message"
                       "--improve-message-cursor-position" cursor-pos
                       (when arg "--head")))
           (output (apply #'commitothy--run args)))
      (erase-buffer)
      (commitothy--insert-output output))))

;;;###autoload
(defun commitothy-show-code-review-for-rev (rev)
  "Run commitothy code review for a given REV string and display it.

When called interactively without a prefix argument, use the commit
at point if in a magit buffer. Otherwise, prompt for a revision.

With a prefix argument, always prompt for a revision.

The prompt will use `magit-read-range-or-commit' when available,
falling back to a regular minibuffer read."
  (interactive
   (let ((prompt-for-rev (lambda ()
                           (if (fboundp 'magit-read-range-or-commit)
                               (magit-read-range-or-commit "Code review for rev: ")
                             (read-string "Code review for rev: ")))))
     (list
      (if current-prefix-arg
          (funcall prompt-for-rev)
        (or (when (fboundp 'magit-commit-at-point) (magit-commit-at-point))
            (funcall prompt-for-rev))))))
  (let ((output (commitothy--run "--code-review" "--rev" rev)))
    (if (string-empty-p output)
        (message "commitothy returned no output.")
      (let* ((review-start (string-match "# \\*\\*\\* CODE REVIEW" output))
             (review-content (when review-start
                               (substring-no-properties output review-start)))
             (cleaned-review (when review-content
                               (replace-regexp-in-string "^# ?" "" review-content))))
        (if cleaned-review
            (with-current-buffer (get-buffer-create (format "*commitothy-review: %s*" rev))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert cleaned-review)
                (markdown-mode)
                (setq-local buffer-read-only t)
                (define-key (current-local-map) (kbd "q") 'bury-buffer))
              (pop-to-buffer (current-buffer)))
          (message "Could not find code review in commitothy output."))))))

(provide 'commitothy)
;;; commitothy.el ends here
