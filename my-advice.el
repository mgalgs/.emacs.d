;; set mark when we go backward-up-list:
(defadvice backward-up-list (around my-backward-up-list-advice activate)
  "stuffs"
  (push-mark (point) t)
  ad-do-it)

(defadvice sp-backward-up-sexp (around my-sp-backward-up-sexp-advice activate)
  "stuffs"
  (push-mark (point) t)
  ad-do-it)

;; (defadvice article-fill-long-lines (around my-article-fill-long-lines-advice activate)
;;   "override `fill-column' due to the way article-fill-long-lines
;;   uses it"
;;   (let ((fill-column (- (window-width (get-buffer-window (current-buffer)))
;;                         5)))
;;     ad-do-it))

(defadvice shell-command (around my-shell-command-advice activate)
  "Put `*Shell Command Output*' buffers into `view-mode'."
  ad-do-it
  (when (get-buffer "*Shell Command Output*")
    (with-current-buffer "*Shell Command Output*"
      (view-mode))))

;; Shrink *Occur* buffer if smallish:
(defadvice occur (around my-occur-advice activate)
  "Make *Occur* buffer small if possible"
  ad-do-it
  (my-switch-to-buffer-and-shrink "*Occur*"))

;; Some advice to recenter after moving to next compile error
(defadvice next-error (after my-next-error-after
				 activate)
  "Recenter the page after next-error"
  (recenter))

(defmacro my-make-recentering-advice (after-what)
  "Macro to define advice to `recenter' after AFTER-WHAT"
  (let ((advsymbol (intern (concat "make-recenter-" (symbol-name after-what))))
	(after-what-str after-what))
    `(defadvice ,after-what (after advsymbol
				    activate)
       "Recenter the page after doing this thing"
       ;; (message (concat "Running advice for " (symbol-name ',after-what-str)))
       (recenter))))

(my-make-recentering-advice find-tag)
(my-make-recentering-advice pop-tag-mark)

(advice-add #'backward-page :after #'recenter)
(advice-add #'forward-page  :after #'recenter)
