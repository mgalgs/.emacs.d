;;;; mailto-compose-mail.el (2010-08-15)

;;;###autoload
(defun mailto-compose-mail (mailto-url)
  "Parse MAILTO-URL and start composing mail."
  (if (and (stringp mailto-url)
           (string-match "\\`mailto:" mailto-url))
      (progn
        (require 'rfc2368)
        (require 'rfc2047)
        (require 'mailheader)

        (let ((hdr-alist (rfc2368-parse-mailto-url mailto-url))
              (body "")
              to subject
              ;; In addition to To, Subject and Body these headers are
              ;; allowed:
              (allowed-xtra-hdrs '(cc bcc in-reply-to)))

          (with-temp-buffer
            ;; Extract body if it's defined
            (when (assoc "Body" hdr-alist)
              (dolist (hdr hdr-alist)
                (when (equal "Body" (car hdr))
                  (insert (format "%s\n" (cdr hdr)))))
              (rfc2047-decode-region (point-min) (point-max))
              (setq body (buffer-substring-no-properties
                          (point-min) (point-max)))
              (erase-buffer))

            ;; Extract headers
            (dolist (hdr hdr-alist)
              (unless (equal "Body" (car hdr))
                (insert (format "%s: %s\n" (car hdr) (cdr hdr)))))
            (rfc2047-decode-region (point-min) (point-max))
            (goto-char (point-min))
            (setq hdr-alist (mail-header-extract-no-properties)))

          (setq to (or (cdr (assq 'to hdr-alist)) "")
                subject (or (cdr (assq 'subject hdr-alist)) "")
                hdr-alist
                (remove nil (mapcar
                             #'(lambda (item)
                                 (when (memq (car item) allowed-xtra-hdrs)
                                   (cons (capitalize (symbol-name (car item)))
                                         (cdr item))))
                             hdr-alist)))

          (compose-mail to subject hdr-alist nil nil
                        (list (lambda (string)
                                (insert string))
                              body))))
    (compose-mail)))
