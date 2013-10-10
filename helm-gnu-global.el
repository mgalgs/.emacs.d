(defun helm-gnu-global-get-symbols ()
  '(("a" . "a")
    ("b" . "b")
    ("c" . "c")))

(setq helm-source-gnu-global-symbols
      '((name . "Gnu Global Symbols")
        (candidates . helm-gnu-global-get-symbols)))

(defun helm-gnu-global ()
  (interactive)
  (helm :sources '(helm-source-gnu-global-symbols)
        :buffer "*helm gnu global*"))
