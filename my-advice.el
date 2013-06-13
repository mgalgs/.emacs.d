;; set mark when we go backward-up-list:
(defadvice backward-up-list (around my-backward-up-list-advice activate)
  "stuffs"
  (push-mark (point) t)
  ad-do-it)

(defadvice article-fill-long-lines (around my-article-fill-long-lines-advice activate)
  "override `fill-column' due to the way article-fill-long-lines
  uses it"
  (let ((fill-column (window-width (get-buffer-window (current-buffer)))))
    ad-do-it))
