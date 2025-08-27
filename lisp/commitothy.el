;;; commitothy.el --- Commit message generation via commitothy -*- lexical-binding: t; -*-

(defcustom commitothy-executable "~/src/commitothy/commitothy.py"
  "Path to the commitothy Python script."
  :type 'file
  :group 'commitothy)

(defcustom commitothy-model "qwen/qwen3-235b-a22b-2507"
  "LLM to use (passed to commitothy as `--model')."
  :type 'string
  :group 'commitothy)

(defun commitothy--run (&rest args)
  "Run commitothy.py with ARGS and return its output string."
  (let* ((args (delq nil args))
         (command (mapconcat #'shell-quote-argument
                             (cons (expand-file-name commitothy-executable)
                                   (append (list "--model" commitothy-model)
                                           args))
                             " ")))
    (message "Running commitothy cmd: %s" command)
    (shell-command-to-string command)))

(defun commitothy--insert-output (output)
  "Insert OUTPUT at point and fill paragraphs."
  (let ((start (point)))
    (insert output)
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

(provide 'commitothy)
;;; commitothy.el ends here
