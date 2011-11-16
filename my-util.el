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
