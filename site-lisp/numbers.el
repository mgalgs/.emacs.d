(require 'calc-bin)

;; TODO: generate these with a macro...

(defun dec2hex (dec-number)
  "Convert `dec-number' to hex string"
  (interactive "sDecimal Number: ")
  (when (stringp dec-number)
    (setq dec-number (string-to-number dec-number)))
  (let ((res (format "0x%x" dec-number)))
    (if (called-interactively-p)
        (message res)
      res)))

(defun hex2dec (hex-number)
  "Convert `hex-number' to a decimal number"
  (interactive "sHex Number: ")
  (when (stringp hex-number)
    (setq hex-number (string-to-number (s-chop-prefix "0x" hex-number) 16)))
  (let ((res (format "%d" hex-number)))
    (if (called-interactively-p)
        (message res)
      (string-to-number res))))

(defun hex2bin (hex)
  "Convert `hex' to a binary string"
  (interactive "sHex Number: ")
  (setq hex (hex2dec hex))
  (let* ((calc-number-radix 2)
         (res (math-format-radix hex)))
    (if (called-interactively-p)
        (message res)
      res)))

(defun hex2mb (hex)
  "Convert `hex' to MB"
  (interactive "sHex Number: ")
  (let* ((bytes (hex2dec hex))
         (res (/ (/ bytes 1024.0) 1024.0)))
    (if (called-interactively-p)
        (message "%g" res)
      res)))

(provide 'numbers)
