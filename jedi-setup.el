;; Note to future self: if jedi isn't working on a new setup, make
;; sure you've done everything in site-lisp/emacs-jedi/README.rst
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)
