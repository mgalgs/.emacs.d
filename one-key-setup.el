;;;; the alist defvars
(defvar one-key-menu-misc-alist nil
  "`One-Key' menu list for misc stuff.")

(defvar one-key-menu-rfc-alist nil
  "`One-Key' menu list for rfc view stuffs.")

;;;; the alists
(setq one-key-menu-misc-alist
      '(
        (("d" . "Lookup word") . my-lookup-current-word)
        (("u" . "Toggle tab width") . my-toggle-tab-width-setting)
        (("i" . "Kill where I am") . kill-where-i-am)
        (("g" . "Grep what I'm on") . grep-what-im-on)
        (("w" . "Make this buffer writable") . my-make-this-buffer-writable)
        (("k" . "Kill buffer and window") . kill-buffer-and-window)
        (("m" . "Woman") . woman)
        (("s" . "Search all buffers") . my-search-all-buffers)))

(setq one-key-menu-rfc-alist
      '(
        (("r" . "Get and view RFC") . get-rfc-view-rfc)
        (("." . "Get and view RFC at point") . get-rfc-view-rfc-at-point)
        (("g" . "Grep RFC index") . get-rfc-grep-rfc-index)))

;;;; the menu functions
(defun one-key-menu-misc ()
  "`One-Key' menu for miscellaneous stuff."
  (interactive)
  (one-key-menu "Misc" one-key-menu-misc-alist t))

(defun one-key-menu-rfc ()
  "`One-Key' menu for rfc view stuffs."
  (interactive)
  (one-key-menu "RFC" one-key-menu-rfc-alist t))


(setq one-key-toplevel-alist
      '(
        (("m" . "misc") . one-key-menu-misc)
        (("r" . "RFC") . one-key-menu-rfc)
        (("k" . "one-key") . one-key-menu-one-key)))
