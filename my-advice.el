;; set mark when we go backward-up-list:
(defadvice backward-up-list (around my-backward-up-list-advice activate)
  "stuffs"
  (push-mark (point) t)
  ad-do-it)
