;;; gptel-tools.el --- Tool functions for GPTel to edit buffers/files -*- lexical-binding: t; -*-

(require 'gptel)

(defun m/gptel-tools--resolve-buffer (bufname)
  "Return buffer object for BUFNAME, creating file-visiting buffer if needed."
  (or (get-buffer bufname)
      (find-file-noselect bufname)))  ;; open file if a path

;; -- Tools --

(defun m/gptel-tool-replace-region (start end text &optional buffer)
  "Replace region [START,END] with TEXT in BUFFER or current buffer."
  (let ((buf (if buffer
                 (m/gptel-tools--resolve-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (delete-region start end)
        (goto-char start)
        (insert text)))
    (format "Replaced region %d–%d in %s" start end (buffer-name buf))))

(defun m/gptel-tool-find-and-replace (old-text new-text &optional buffer start end)
  "Find and replace OLD-TEXT with NEW-TEXT in BUFFER (or current buffer).
Replaces all occurrences by default. If START and END are provided,
constrain the search to the region [START, END]."
  (let ((buf (if buffer
                 (m/gptel-tools--resolve-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (or start (point-min)))
        (let ((count 0)
              (search-end (or end (point-max))))
          (while (and (< (point) search-end)
                      (search-forward old-text search-end t))
            (replace-match new-text t t)
            (setq count (1+ count)))
          (format "Replaced %d occurrence(s) of '%s' in %s"
                  count
                  (truncate-string-to-width old-text 20 nil nil "...")
                  (buffer-name buf)))))))

(defun m/gptel-tool-read-buffer (&optional buffer start end)
  "Return buffer content as string.
If BUFFER is specified, read from that buffer, otherwise current buffer.
If START and END are specified, read that region, otherwise entire buffer."
  (let ((buf (if buffer
                 (m/gptel-tools--resolve-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (buffer-substring-no-properties (or start (point-min)) (or end (point-max))))))

(defun m/gptel-tool-find-in-file (search-text &optional buffer start end)
  "Find SEARCH-TEXT in BUFFER (or current buffer) and return positions.
Returns list of (start-pos end-pos) for each match.
If START and END are specified, constrain search to that region."
  (let ((buf (if buffer
                 (m/gptel-tools--resolve-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (or start (point-min)))
        (let ((matches '())
              (search-end (or end (point-max))))
          (while (and (< (point) search-end)
                      (search-forward search-text search-end t))
            (push (list (match-beginning 0) (match-end 0)) matches))
          (format "Matches: %s" (nreverse matches)))))))

(defun m/gptel-tool-find-in-file-regexp (regexp &optional buffer start end)
  "Find REGEXP in BUFFER (or current buffer) and return positions.
Returns list of (start-pos end-pos) for each match.
If START and END are specified, constrain search to that region."
  (let ((buf (if buffer
                 (m/gptel-tools--resolve-buffer buffer)
               (current-buffer))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (or start (point-min)))
        (let ((matches '())
              (search-end (or end (point-max))))
          (while (and (< (point) search-end)
                      (re-search-forward regexp search-end t))
            (push (list (match-beginning 0) (match-end 0)) matches))
          (format "Matches: %s" (nreverse matches)))))))

(defun m/gptel-tool-list-buffers ()
  "Return a list of live buffer names."
  (format "Buffer names: %s" (mapcar #'buffer-name (buffer-list))))

(defun m/gptel-tool-describe-variable (name)
  "Return `describe-variable' output (string) for variable NAME."
  (let ((sym (intern-soft name)))
    (if (and sym (boundp sym))
        (let ((doc (documentation-property sym 'variable-documentation))
              (value (symbol-value sym)))
          (format "Variable: %s\n\nValue: %s\n\n%s"
                  name
                  (if (stringp value) (prin1-to-string value) value)
                  (or doc "No documentation.")))
      (format "Variable %s not found" name))))

(defun m/gptel-tool-describe-function (name)
  "Return `describe-function' output (string) for function NAME."
  (let ((sym (intern-soft name)))
    (if (and sym (fboundp sym))
        (let ((doc (documentation sym t))
              (arglist (help-function-arglist sym)))
          (format "Function: %s\n\nArguments: %s\n\n%s"
                  name
                  (or arglist "unknown")
                  (or doc "No documentation.")))
      (format "Function %s not found" name))))

;; --- Web Browsing Tool ---
(defun m/gptel-tool-browse-web (url)
  "Fetch URL contents as plain text for use as a GPT tool."
  (require 'url)
  (require 'shr)
  (with-current-buffer (url-retrieve-synchronously url t t 10)
    (goto-char (point-min))
    (re-search-forward "\n\n" nil 'move)
    (let ((dom (libxml-parse-html-region (point) (point-max))))
      (with-temp-buffer
        (shr-insert-document dom)
        (buffer-string)))))

;; ---- Export tools ----
(defun m/get-gptel-tools ()
  "Return a list of tools for gptel to use for buffer and file manipulation.

Can be used like so: (setq gptel-tools (m/get-gptel-tools))"
  (list
   (gptel-make-tool
    :name "replace_region"
    :function 'm/gptel-tool-replace-region
    :description "Replace text in a region"
    :args (list '(:name "start" :type integer :description "Start position")
                '(:name "end"   :type integer :description "End position")
                '(:name "text"  :type string  :description "New text")
                '(:name "buffer" :type string :description "Optional buffer name" :optional t)))
   (gptel-make-tool
    :name "find_and_replace"
    :function 'm/gptel-tool-find-and-replace
    :description "Find and replace text in a buffer"
    :args (list '(:name "old-text" :type string :description "Text to find")
                '(:name "new-text" :type string :description "Replacement text")
                '(:name "buffer" :type string :description "Optional buffer name" :optional t)
                '(:name "start" :type integer :description "Start position for constrained search" :optional t)
                '(:name "end" :type integer :description "End position for constrained search" :optional t)))
   (gptel-make-tool
    :name "read_buffer"
    :function 'm/gptel-tool-read-buffer
    :description "Read buffer content as string"
    :args (list '(:name "buffer" :type string :description "Optional buffer name" :optional t)
                '(:name "start" :type integer :description "Start position for reading" :optional t)
                '(:name "end" :type integer :description "End position for reading" :optional t)))
   (gptel-make-tool
    :name "find_in_file"
    :function 'm/gptel-tool-find-in-file
    :description "Find text in buffer and return positions"
    :args (list '(:name "search-text" :type string :description "Text to find")
                '(:name "buffer" :type string :description "Optional buffer name" :optional t)
                '(:name "start" :type integer :description "Start position for constrained search" :optional t)
                '(:name "end" :type integer :description "End position for constrained search" :optional t)))
   (gptel-make-tool
    :name "find_in_file_regexp"
    :function 'm/gptel-tool-find-in-file-regexp
    :description "Find regexp in buffer and return positions"
    :args (list '(:name "regexp" :type string :description "Regular expression to find")
                '(:name "buffer" :type string :description "Optional buffer name" :optional t)
                '(:name "start" :type integer :description "Start position for constrained search" :optional t)
                '(:name "end" :type integer :description "End position for constrained search" :optional t)))
   (gptel-make-tool
    :name "list_buffers"
    :function 'm/gptel-tool-list-buffers
    :description "List open buffer names"
    :args nil)
   (gptel-make-tool
    :name "describe_variable"
    :function 'm/gptel-tool-describe-variable
    :description "Get value for an Emacs Lisp variable."
    :args (list '(:name "name" :type string :description "Variable name")))
   (gptel-make-tool
    :name "describe_function"
    :function 'm/gptel-tool-describe-function
    :description "Get docstring for an Emacs Lisp function."
    :args (list '(:name "name" :type string :description "Function name")))
   (gptel-make-tool
    :name "browse_web"
    :function 'm/gptel-tool-browse-web
    :description "Fetch the plain text content of a webpage via URL."
    :args (list '(:name "url" :type string :description "URL of the page to fetch")))))

(provide 'gptel-tools)
;;; gptel-tools.el ends here
