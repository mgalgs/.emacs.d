(require 'which-c-preprocessor-cond)
(which-c-preprocessor-cond-mode)

(setq-default header-line-format
              `((which-func-mode ("" which-func-format " "))
                (which-c-preprocessor-cond-mode ,which-c-preprocessor-cond-format)))
