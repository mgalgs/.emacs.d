(require 'helm)
(require 'helm-config)
(require 'helm-ls-git)
(require 'helm-swoop)
(require 'helm-gnu-global)
(require 'helm-gtags)
(require 'helm-bookmark)
(setq helm-minibuffer-history-key nil)
(setq helm-truncate-lines t)
;; todo: make cycling global (not per-source):
;; https://github.com/emacs-helm/helm/issues/387
(setq helm-move-to-line-cycle-in-source nil)
