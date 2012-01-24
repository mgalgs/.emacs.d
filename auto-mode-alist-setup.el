(add-to-list 'auto-mode-alist '("/crontab.*$" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))
(add-to-list 'auto-mode-alist '("\\.js\\'" . espresso-mode))
(add-to-list 'auto-mode-alist
             '("/\\(rfc\\|std\\)[0-9]+\\.txt\\'" . rfcview-mode))
(add-to-list 'auto-mode-alist '(".bc\\'" . bc-mode))

(add-to-list 'auto-mode-alist '("/COMMIT_EDITMSG$" . conf-mode))

(add-to-list 'auto-mode-alist '("\\.[bB][aA][tT]$" . bat-mode))

;; Magically go into c++ mode (for windriver headers):
(add-to-list 'magic-fallback-mode-alist '("^// " . c++-mode))

(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.x$" . c++-mode))
