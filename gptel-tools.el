;;; gptel-tools.el --- Tool functions for GPTel to edit buffers/files -*- lexical-binding: t; -*-

(require 'gptel)

(defun m/gptel-tools--resolve-buffer (bufname)
  "Return buffer object for BUFNAME, creating file-visiting buffer if needed."
  (or (get-buffer bufname)
      (find-file-noselect bufname)))  ;; open file if a path

;; -- Tools --

(defun m/gptel-tool-insert (text)
  "Insert TEXT at point in current buffer."
  (insert text)
  (format "Inserted %d chars into %s" (length text) (buffer-name)))

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

(defun m/gptel-tool-open-file (path)
  "Open file PATH and return buffer name."
  (buffer-name (find-file-noselect path)))

(defun m/gptel-tool-save-buffer (buffer)
  "Save BUFFER to disk."
  (let ((buf (m/gptel-tools--resolve-buffer buffer)))
    (with-current-buffer buf
      (save-buffer))
    (format "Saved %s" (buffer-name buf))))

(defun m/gptel-tool-list-buffers ()
  "Return a list of live buffer names."
  (mapcar #'buffer-name (buffer-list)))

(defun m/gptel-tool-describe-variable (name)
  "Return `describe-variable' output (string) for variable NAME."
  (let ((sym (intern-soft name)))
    (if (and sym (boundp sym))
        (with-temp-buffer
          (format "Function %s not found" name)))))

;; ---- Export tools ----
(defun m/get-gptel-tools ()
  "Return a list of tools for gptel to use for buffer and file manipulation.

Can be used like so: (setq gptel-tools (m/get-gptel-tools))"
  (list
   (gptel-make-tool
    :name "insert_text"
    :function #'m/gptel-tool-insert
    :description "Insert given text at point in the current buffer"
    :args (list '(:name "text" :type string :description "Text to insert")))
   (gptel-make-tool
    :name "replace_region"
    :function #'m/gptel-tool-replace-region
    :description "Replace text in a region"
    :args (list '(:name "start" :type integer :description "Start position")
                '(:name "end"   :type integer :description "End position")
                '(:name "text"  :type string  :description "New text")
                '(:name "buffer" :type string :description "Optional buffer name" :optional t)))
   (gptel-make-tool
    :name "open_file"
    :function #'m/gptel-tool-open-file
    :description "Open a file into a buffer"
    :args (list '(:name "path" :type string :description "Path to file")))
   (gptel-make-tool
    :name "save_buffer"
    :function #'m/gptel-tool-save-buffer
    :description "Save a buffer to disk"
    :args (list '(:name "buffer" :type string :description "Buffer name")))
   (gptel-make-tool
    :name "list_buffers"
    :function #'m/gptel-tool-list-buffers
    :description "List open buffer names"
    :args nil)
   (gptel-make-tool
    :name "describe_variable"
    :function #'m/gptel-tool-describe-variable
    :description "Get value for an Emacs Lisp variable."
    :args (list '(:name "name" :type string :description "Variable name")))
   (gptel-make-tool
    :name "describe_function"
    :function #'m/gptel-tool-describe-function
    :description "Get docstring for an Emacs Lisp function."
    :args (list '(:name "name" :type string :description "Function name")))))

(provide 'gptel-tools)
;;; gptel-tools.el ends here
