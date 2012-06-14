(require 'cl)

(defun directories-in-directory (directory)
  "Returns a list of all directories contained in DIRECTORY"
  (let* ((all-files (mapcar (lambda (el)
                              (concat (file-name-as-directory directory) el))
                            (directory-files directory)))
         ;; remove '.', '..' and all non-directories
         (filtered-files (remove-if (lambda (el)
                                      (message "testing %s (last char is %s)" el (substring el -1))
                                      (if (file-directory-p el) (message "%s is a dir" el))
                                      (or (string= "." (substring el -1))
                                          (not (file-directory-p el))))
                                    all-files)))
    filtered-files))
