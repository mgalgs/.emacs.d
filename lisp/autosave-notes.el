;;; autosave-notes.el --- Autosave *notes* buffer module -*- lexical-binding: t; -*-

(defvar autosave-notes-buffer-name "*notes*"
  "Name of the notes buffer to autosave.")

(defvar autosave-notes-file "~/tmp/notes.txt"
  "File path to save the *notes* buffer.")

(defun autosave-notes-buffer (&optional _begin _end _length)
  "Automatically save the *notes* buffer to a file whenever there is a change.
Arguments _BEGIN, _END, and _LENGTH are passed by `after-change-functions' but unused."
  (when (and (string= (buffer-name) autosave-notes-buffer-name)
             (buffer-modified-p))
    (write-region (point-min) (point-max) autosave-notes-file nil 0)))

;;;###autoload
(define-minor-mode autosave-notes-mode
  "Minor mode to autosave the *notes* buffer on any change."
  :global t
  (if autosave-notes-mode
      (add-hook 'after-change-functions #'autosave-notes-buffer)
    (remove-hook 'after-change-functions #'autosave-notes-buffer)))

;;;###autoload
(defun autosave-notes-open-notes-buffer ()
  "Switch to the *notes* buffer if already open, otherwise open it and
populate its contents from saved file."
  (interactive)
  (let* ((existing (get-buffer autosave-notes-buffer-name))
         (buf (or existing
                  (get-buffer-create autosave-notes-buffer-name))))
    (switch-to-buffer buf)
    (unless existing
      (when (file-readable-p autosave-notes-file)
        ;; Temporarily disable autosave hook while inserting
        (let ((after-change-functions nil))
          (erase-buffer)
          (insert-file-contents autosave-notes-file))))
    (set-buffer-modified-p nil)))

(provide 'autosave-notes)

;;; autosave-notes.el ends here
