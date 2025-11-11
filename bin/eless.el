;; -*- lexical-binding: t -*-
;;; eless.el --- Ephemerally view file contents in Emacs

(defun eless (file)
  "Open FILE in a *eless* buffer, delete temp file on quit."
  (let* ((buf (get-buffer-create "*eless*"))
         (map (make-sparse-keymap))
         (eless-session-cleanup (lambda ()
                                  (interactive)
                                  (when (file-exists-p file)
                                    (delete-file file))
                                  (kill-buffer buf)
                                  (save-buffers-kill-terminal))))
    (define-key map (kbd "q") eless-session-cleanup)
    (define-key map (kbd "C-x C-c") eless-session-cleanup)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents file))
      ;; install override first
      (setq-local minor-mode-overriding-map-alist `((view-mode . ,map)))
      ;; now enable view-mode
      (view-mode 1))
    (switch-to-buffer buf)))

(provide 'eless)
;;; eless.el ends here
