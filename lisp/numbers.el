(require 's)
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

(defun dec2bin (dec-number)
  "Convert `dec-number' to binary string"
  (interactive "sDecimal Number: ")
  (let ((res (hex2bin (dec2hex dec-number))))
    (if (called-interactively-p)
        (message res))
    res))

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

(defun hex2gb (hex)
  "Convert `hex' (in bytes) to GB"
  (interactive "sHex Number: ")
  (dec2gb (hex2dec hex) (called-interactively-p)))

(defun hex2mb (hex)
  "Convert `hex' (in bytes) to MB"
  (interactive "sHex Number: ")
  (dec2mb (hex2dec hex) (called-interactively-p)))

(defun hex2kb (hex)
  "Convert `hex' (in bytes) to KB"
  (interactive "sHex Number: ")
  (dec2kb (hex2dec hex) (called-interactively-p)))

(defun dec2gb (dec &optional force-print)
  "Convert `dec' (in bytes) to GB"
  (interactive "nDec Number: ")
  (let ((res (/ (/ (/ dec 1024.0) 1024.0) 1024.0)))
    (if (or (called-interactively-p) force-print)
        (message "%g" res)
      res)))

(defun dec2mb (dec &optional force-print)
  "Convert `dec' (in bytes) to MB"
  (interactive "nDec Number: ")
  (let ((res (/ (/ dec 1024.0) 1024.0)))
    (if (or (called-interactively-p) force-print)
        (message "%g" res)
      res)))

(defun dec2kb (dec &optional force-print)
  "Convert `dec' (in bytes) to KB"
  (interactive "nDec Number: ")
  (let ((res (/ dec 1024.0)))
    (if (or (called-interactively-p) force-print)
        (message "%g" res)
      res)))

(defun bin2dec (bin)
  "Convert `bin' to a decimal number"
  (interactive "sBinary Number: ")
  (when (stringp bin)
    (setq bin (string-to-number (s-chop-prefix "0b" bin) 2)))
  (let ((res (format "%d" bin)))
    (if (called-interactively-p)
        (message res)
      (string-to-number res))))

(defun bin2hex (bin)
  "Convert `bin' to a hex string."
  (interactive "sBinary Number: ")
  (setq bin (bin2dec bin))
  (let* ((calc-number-radix 16)
         (res (concat "0x" (math-format-radix bin))))
    (if (called-interactively-p)
        (message res)
      res)))

(provide 'numbers)
