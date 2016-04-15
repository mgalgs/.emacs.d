(require 'package)

;; don't let package init things. we'll use use-package for that.
(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(use-package paradox)
(setq paradox-execute-asynchronously t)
