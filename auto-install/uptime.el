;;; uptime.el: keeps a persistent record of your best emacs-uptime.

(unless (file-exists-p "~/.emacs.d/.uptime")
  (let ((current-uptime (string-to-number (emacs-uptime "%s"))))
    (save-excursion
      (let ((buf (find-file-noselect "~/.emacs.d/.uptime")))
	(set-buffer buf)
	(print (list 'setq 'best-uptime current-uptime) buf)
	(save-buffer)
	(kill-buffer)))))

(load "~/.emacs.d/.uptime")

(defun save-uptime ()
  "Save the best uptime into .uptime."
  (save-excursion
    (let ((buf (find-file-noselect "~/.emacs.d/.uptime")))
      (set-buffer buf)
      (erase-buffer)
      (print (list 'setq 'best-uptime (string-to-number (emacs-uptime "%s"))) buf)
      (save-buffer 0)
      (kill-buffer))))

(defun compare-uptimes ()
  "Update *best-uptime* if necessary."
  (let ((uptime (string-to-number (emacs-uptime "%s"))))
    (if (> uptime best-uptime)
	(save-uptime))))
	  
(add-hook 'kill-emacs-hook 'compare-uptimes)

(defun uptime ()
  "Display best uptime in mini-buffer"
  (interactive)
  (compare-uptimes)
  (load "~/.emacs.d/.uptime")
  (let ((current-uptime (string-to-number (emacs-uptime "%s"))))
    (if (< current-uptime best-uptime)
	(message (format "Your current uptime is %s, but your best uptime is %s" 
			 (format-seconds "%D, %z%.2h:%.2m:%.2s" current-uptime)
			 (format-seconds "%D, %z%.2h:%.2m:%.2s" best-uptime)))
      (message (format "Your current and best emacs uptime is: %s" 
		       (format-seconds "%D, %z%.2h:%.2m:%.2s" best-uptime))))))
