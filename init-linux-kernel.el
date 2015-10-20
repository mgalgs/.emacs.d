(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
		 (column (c-langelem-2nd-pos c-syntactic-element))
		 (offset (- (1+ column) anchor))
		 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(c-add-style "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))

(defun m/kernel-source-hook ()
  (let ((filename (buffer-file-name)))
    ;; Enable kernel mode for the appropriate files
    (if (and filename
               (or (string-match (expand-file-name "/local/mnt/workspace/mitchelh/.*/kernel")
                                 filename)
                   (string-match "/local/mnt/workspace/mitchelh/msm-kvm"
                                 filename)
                   (locate-dominating-file filename "Kbuild")
                   (locate-dominating-file filename "Kconfig")
                   (save-excursion (goto-char 0)
                                   (search-forward-regexp "^#include <linux/\\(module\\|kernel\\)\\.h>$" nil t))))
        (progn
          (setq indent-tabs-mode t)
          (setq tab-width 8)
          (setq c-basic-offset 8)
          (message "Setting up indentation for the linux kernel")
          (c-set-style "linux"))
      (c-set-style "k&r"))))

(add-hook 'c-mode-hook 'm/kernel-source-hook)
