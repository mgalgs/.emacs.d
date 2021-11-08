(require 'json)
(require 's)

(defcustom quick-clone-target-directory "~/src"
  "The directory where repos should be cloned.")

(setq qc--ghapi-search-url "https://api.github.com/search/repositories")

(defun qc--search-repos (q)
  (message "Searching github for %s" q)
  (with-current-buffer (url-retrieve-synchronously (concat qc--ghapi-search-url
                                                           "?q="
                                                           (url-encode-url q)))
    (goto-char (1+ url-http-end-of-headers))
    (let* ((results (json-parse-string (buffer-substring-no-properties (point) (point-max)))))
      ;; mapcar to convert the vector to a list
      (mapcar 'identity (gethash "items" results)))))

(defun qc--do-search (q)
  (if (string= q "")
      '()
    (mapcar (lambda (repo) (format "%s %s %s"
                                   (gethash "full_name" repo)
                                   (gethash "clone_url" repo)
                                   (gethash "description" repo)))
            (qc--search-repos q))))

(defun quick-clone ()
  (interactive)
  (let ((ivy-dynamic-exhibit-delay-ms 500))
    (ivy-read "Clone repo: " 'qc--do-search
              :dynamic-collection t
              :action (lambda (result)
                        (let* ((parts (split-string result " "))
                               (name (nth 0 parts))
                               (clone-url (nth 1 parts))
                               (clone-dirname (nth 0
                                                   (last (split-string (s-chop-suffix ".git"
                                                                                      clone-url)
                                                                       "/"))))
                               (full-clone-dir (concat (file-name-as-directory (expand-file-name quick-clone-target-directory))
                                                       clone-dirname)))
                          (if (file-exists-p full-clone-dir)
                              (message "%s already exists. Visiting." full-clone-dir)
                            (message "Will clone %s from %s to %s" name clone-url full-clone-dir)
                            (shell-command (format "git clone %s %s"
                                                   clone-url
                                                   full-clone-dir)))
                          (magit-status full-clone-dir))))))

(provide 'quick-clone)
