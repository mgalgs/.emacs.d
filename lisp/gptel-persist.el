;;; gptel-persist.el --- Persistent sessions for gptel -*- lexical-binding: t; -*-

;; Author: Mitchel Humpherys <mitch.special@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (gptel "0.8"))
;; Keywords: convenience, chat, AI
;; URL: https://github.com/mgalgs/gptel-persist

;;; Commentary:
;; Provides simple autosave and restore functionality for gptel sessions.
;; Sessions are saved to ~/.emacs.d/gptel-persist-cache/data.eld.
;; Use `M-x gptel-persist-restore` to pick and reopen a past session.
;;
;; Persistent session support for [gptel](https://github.com/karthink/gptel).
;;
;; Automatically saves and restores your conversations between Emacs sessions, using
;; a throttled autosave-on-change mechanism.
;;
;; ## Installation
;;
;; Put `gptel-persist.el` somewhere in your Emacs `load-path` and `(require 'gptel-persist)`.
;;
;; Example using `use-package`:
;;
;; ```emacs-lisp
;; (use-package gptel-persist
;;   :load-path "~/path/to/" ; wherever gptel-persist.el is located
;;   :after gptel
;;   :ensure nil ;; local file
;;   :config
;;   (gptel-persist-enable))
;; ```
;;
;; ## Usage
;;
;; - Every `gptel` buffer autosaves its session to
;;   `~/.emacs.d/gptel-cache/data.eld`.
;; - Restore with:
;;   `M-x gptel-persist-restore`
;; - Pick past sessions using completing-read.
;; - The same model and conversation messages are restored into a new
;;   gptel session.
;;
;; ## Notes
;;
;; - This uses a 1-second idle delay to avoid saving on every keystroke.
;; - Each session is keyed by buffer name.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'subr-x)

(defgroup gptel-persist nil
  "Persistent gptel sessions."
  :group 'gptel)

(defcustom gptel-persist-dir
  (expand-file-name "gptel-persist-cache/" user-emacs-directory)
  "Directory to store gptel session data."
  :type 'directory)

(defcustom gptel-persist-file
  (expand-file-name "data.eld" gptel-persist-dir)
  "File where session data are persisted."
  :type 'file)

(defvar gptel-persist--sessions (make-hash-table :test 'equal))

;;;; Internal

(defun gptel-persist--make-uuid ()
  "Generate a random UUID v4-like string (no external deps)."
  (format "%08x-%04x-%04x-%04x-%012x"
          (random (expt 16 8))
          (random (expt 16 4))
          (logior #x4000 (random #x1000))
          (logior #x8000 (random #x4000))
          (random (expt 16 12))))

(defvar-local gptel-persist--uuid nil)

(defun gptel-persist--ensure-uuid ()
  (or gptel-persist--uuid
      (setq gptel-persist--uuid (gptel-persist--make-uuid))))

(defun gptel-persist--save-file ()
  "Persist all session data, including UUID keys."
  (make-directory gptel-persist-dir t)
  (with-temp-file gptel-persist-file
    (let (out)
      (maphash (lambda (uuid data)
                 (push (plist-put data :uuid uuid) out))
               gptel-persist--sessions)
      (prin1 out (current-buffer)))))

(defun gptel-persist--serialize (buffer)
  "Persist BUFFER's visible contents and model/backend metadata to disk."
  (when (and (buffer-live-p buffer)
             (bound-and-true-p gptel-mode))
    (with-current-buffer buffer
      (let* ((uuid (gptel-persist--ensure-uuid))
             (name (buffer-name buffer))
             (model (or (ignore-errors gptel-model) "unknown"))
             (backend (ignore-errors (when (boundp 'gptel-backend)
                                       (gptel-backend-name gptel-backend))))
             (temperature (ignore-errors gptel-temperature))
             (system (ignore-errors gptel--system-message))
             (tokens (ignore-errors gptel-max-tokens))
             (contents (buffer-substring-no-properties (point-min) (point-max)))
             (data (list :buffer name
                         :model model
                         :backend backend
                         :temperature temperature
                         :system system
                         :tokens tokens
                         :contents contents
                         :timestamp (format-time-string "%FT%T%z"))))
        (puthash uuid data gptel-persist--sessions)
        (gptel-persist--save-file)))))

(defvar-local gptel-persist--save-timer nil)

(defun gptel-persist--schedule-save ()
  "Schedule a save, canceling any previous pending one."
  (when gptel-persist--save-timer
    (cancel-timer gptel-persist--save-timer))
  (setq gptel-persist--save-timer
        (run-with-idle-timer
         1 nil (lambda (buf)
                 (when (buffer-live-p buf)
                   (gptel-persist--serialize buf))
                 (setq gptel-persist--save-timer nil))
         (current-buffer))))

(defun gptel-persist--autosave-change (&rest _)
  (when (bound-and-true-p gptel-mode)
    (gptel-persist--schedule-save)))

(defun gptel-persist--load ()
  (when (file-readable-p gptel-persist-file)
    (with-temp-buffer
      (insert-file-contents gptel-persist-file)
      (setq gptel-persist--sessions
            (let ((tbl (make-hash-table :test 'equal)))
              (dolist (it (condition-case err
                              (read (current-buffer))
                            (error
                             (message "gptel-persist: failed to read %s (%s)"
                                      gptel-persist-file (error-message-string err))
                             nil)))
                (when it
                  (puthash (plist-get it :uuid) it tbl)))
              tbl)))))

;;;;###autoload
(defun gptel-persist-restore ()
  "Select a saved gptel session and restore its metadata/content."
  (interactive)
  (gptel-persist--load)
  (if-let* ((uuids (hash-table-keys gptel-persist--sessions))
            ;; Gather data, then sort by timestamp (newest first)
            (data-list (mapcar (lambda (u) (gethash u gptel-persist--sessions)) uuids))
            (sorted
             (seq-sort-by
              (lambda (data) (date-to-time (plist-get data :timestamp)))
              #'time-less-p
              data-list))
            (sorted (reverse sorted))
            (choices (mapcar (lambda (data)
                               (let* ((label (format-time-string
                                              "[%Y-%m-%d %H:%M] "
                                              (date-to-time (plist-get data :timestamp))))
                                      (first-line (car (split-string (plist-get data :contents) "\n"))))
                                 (cons (truncate-string-to-width
                                        (concat label first-line) 80 nil nil "…")
                                       (plist-get data :uuid))))
                             sorted))
            (choice (cdr (assoc (let ((vertico-sort-function nil))
                                  (completing-read "Restore session: " choices nil t))
                                choices)))
            (data (gethash choice gptel-persist--sessions)))
      (let* ((uuid (plist-get data :uuid))
             (bufname (format "*gptel-%s*" (substring (md5 uuid) 0 8)))
             (buf (generate-new-buffer bufname)))
        (switch-to-buffer buf)
        (gptel buf)
        (setq-local gptel-persist--uuid uuid)
        (setq-local gptel-model       (plist-get data :model))
        (setq-local gptel-temperature (plist-get data :temperature))
        (setq-local gptel--system-message (plist-get data :system))
        (setq-local gptel-max-tokens  (plist-get data :tokens))
        (let ((backend-name (plist-get data :backend)))
          (when (and backend-name (fboundp 'gptel-get-backend))
            (setq-local gptel-backend (gptel-get-backend backend-name))))
        (erase-buffer)
        (insert (plist-get data :contents))
        (gptel-persist-mode 1))
    (user-error "No sessions saved")))

;;;###autoload
(define-minor-mode gptel-persist-mode
  "Minor mode for autosaving gptel sessions."
  :lighter " Persist"
  (if gptel-persist-mode
      (add-hook 'after-change-functions #'gptel-persist--autosave-change nil t)
    (remove-hook 'after-change-functions #'gptel-persist--autosave-change t)))

;;;###autoload
(defun gptel-persist-enable ()
  "Enable gptel-persist-mode for all gptel buffers."
  (gptel-persist--load)
  (add-hook 'gptel-mode-hook #'gptel-persist-mode))

(provide 'gptel-persist)
;;; gptel-persist.el ends here
