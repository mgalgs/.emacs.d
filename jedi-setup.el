;; Note to future self: if jedi isn't working on a new setup, make
;; sure you've done everything in site-lisp/emacs-jedi/README.rst
;;
;; Namely:
;;    make requirements # (from the jedi directory)


(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)
(autoload 'jedi:setup "jedi" nil t)
