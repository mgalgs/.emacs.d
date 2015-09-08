(require 'xcscope)

(defun my-cscope-setup ()
  "Automatically turns on cscope-minor-mode when editing C and
C++ sources"
  (interactive)
  (add-hook 'c-mode-hook (function cscope-minor-mode))
  (add-hook 'c++-mode-hook (function cscope-minor-mode))
  (add-hook 'dired-mode-hook (function cscope-minor-mode))
  (add-hook 'python-mode-hook (function cscope-minor-mode)))

(my-cscope-setup)

(setq cscope-index-recursively t)

(defun my-pycscope-git-project ()
  "Run `cscope-index-files' on the current git project using pycscope"
  (interactive)
  (let ((cscope-program ;; (expand-file-name "~/src/pycscope/venv/bin/pycscope")
                        cscope-program))
    (cscope-index-files (magit-toplevel))))
