;; set up cedet:
;; snapshot from http://www.randomsample.de/cedet-snapshots/
(load-file "~/.emacs.d/site-lisp/cedet-bzr/trunk/common/cedet.el") ;dev version

(global-ede-mode t)
(semantic-load-enable-excessive-code-helpers)
(require 'semantic-ia)

;; cedet helper function (cscope-pop-mark envy)
(defvar semantic-tags-location-ring (make-ring 20))

(defun semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump save the pointer
marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove semantic-tags-location-ring 0))
           (buff (marker-buffer marker))
           (pos (marker-position marker)))
      (if (not buff)
          (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

;; set up cedet keybindings:
(defun my-cedet-hook ()
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c TAB") 'semantic-complete-analyze-inline-idle)
  (local-set-key "\C-ci" 'eassist-switch-h-cpp)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key (kbd "M-.") 'semantic-goto-definition)
  (local-set-key (kbd "M-*") 'semantic-pop-tag-mark))
(add-hook 'c-mode-common-hook 'my-cedet-hook)

;; useful keybindings in semantic Symref mode:
(defun my-semantic-symref-mode-hook ()
  (local-set-key (kbd "TAB") 'forward-button)
  (local-set-key (kbd "<backtab>") 'backward-button))
(add-hook 'semantic-symref-results-mode-hook 'my-semantic-symref-mode-hook)

;; load any cedet projects definitions:
(if (file-exists-p "~/.cedet-projects.el")
    (load-file "~/.cedet-projects.el"))

;; system includes
(require 'semantic-gcc)
