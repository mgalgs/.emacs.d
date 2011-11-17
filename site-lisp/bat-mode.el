;;; bat-mode.el --- Simple mode for Windows BAT files

;; Author: Peter Breton <pbreton@i-kinetics.com> 
;; Created: Thu Jul 25 1996
;; Version: $Id: bat-mode.el,v 1.2 1996/09/27 13:29:52 pbreton Exp pbreton $
;; Keywords: 
;; Time-stamp: <96/09/27 09:36:53 pbreton>

;;; Commentary:
;;
;; USAGE: Byte-compile this file, and add the following lines to your
;;   emacs initialization file (.emacs/_emacs):
;; 
;; (setq auto-mode-alist 
;;       (append 
;;        (list (cons "\\.[bB][aA][tT]$" 'bat-mode))
;;        ;; For DOS init files
;;        (list (cons "CONFIG\\."   'bat-mode))
;;        (list (cons "AUTOEXEC\\." 'bat-mode))
;;        auto-mode-alist))
;;
;;   (autoload 'bat-mode "bat-mode"
;;      "DOS and WIndows BAT files" t)

;; TODO:
;;
;; Support "compiles" ?
;; Imenu? Don't have real functions.....

;;; Change log:
;; $Log: bat-mode.el,v $
;; Revision 1.2  1996/09/27 13:29:52  pbreton
;; Merged with changes from home
;;
;; Revision 1.4  1996/09/27 03:50:24  peter
;; Changed copy-keymap to make-sparse-keymap for dramatic speed improvement
;; Made all keywords case-insensitive
;; Added kill-all-local-variables
;; Changed add-to-list function to setq ... append
;;
;; Revision 1.3  1996/08/22 02:31:47  peter
;; Added Usage message, credit to folks from NTEmacs mailing list,
;; Syntax table, New font-lock keywords
;;
;; Revision 1.2  1996/08/18 16:27:13  peter
;; Added preliminary global-font-lock support
;;
;; Revision 1.1  1996/08/18 16:14:18  peter
;; Initial revision
;;

;; Credit for suggestions, patches and bug-fixes:
;;   Robert Brodersen <rbrodersen@siebel.com>
;;   ACorreir@pervasive-sw.com (Alfred Correira)

;;; Code:

(defvar bat-mode-map nil "Local keymap for bat-mode buffers.")

;; Make this lowercase if you like
(defvar bat-mode-comment-start "REM "
  "Comment string to use in BAT mode")

(defvar bat-mode-syntax-table nil
  "Syntax table in use in Bat-mode buffers.")

(if bat-mode-map
    nil
  (setq bat-mode-map (make-sparse-keymap))
)

;; Make underscores count as words
(if bat-mode-syntax-table
    ()
  (setq bat-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  bat-mode-syntax-table)
)

(defun bat-mode ()
  "Mode for DOS and Windows BAT files"
  (interactive)
  (kill-all-local-variables)
  (use-local-map              bat-mode-map)
  (set-syntax-table	      bat-mode-syntax-table)

  (make-local-variable	     'parse-sexp-ignore-comments)
  (make-local-variable	     'comment-start)
  (make-local-variable	     'comment-start-skip)
  (make-local-variable	     'comment-end)
  (make-local-variable	     'executable-command)
  (make-local-variable	     'font-lock-defaults)

  (setq major-mode          'bat-mode
        mode-name           "bat"

        comment-end         ""

       comment-start       bat-mode-comment-start
       comment-start-skip  "[Rr][Ee][Mm] *"

       parse-sexp-ignore-comments t
       
       )

  ;; Global font-lock support
  ;; (setq font-lock-defaults (list 'bat-font-lock-keywords nil t nil nil))
  (setq font-lock-defaults (list 'bat-font-lock-keywords nil))

  (run-hooks 'bat-mode-hook))

(defvar bat-font-lock-keywords
  (list
   ;; Make this one first in the list, otherwise comments will
   ;; be over-written by other variables
    (list "^[@ \t]*\\([rR][eE][mM].*\\)" 1 'font-lock-comment-face t)
    (list "^[ \t]*\\(::-.*\\)"		 1 'font-lock-comment-face t)
    ;; These keywords appear as the first word on a line
    (list
     (concat "^[@ \t]*\\(\\<"
      (mapconcat 'identity
                 '(
                   "[cC][aA][lL][lL]"
                   "[eE][cC][hH][oO]"
                   "[fF][oO][rR]"
                   "[iI][fF]"
                   "[pP][aA][tT][hH]"
                   "[pP][aA][uU][sS][eE]"
                   "[pP][rR][oO][mM][pP][tT]"
                   "[sS][eE][tT]"
                   "[sS][tT][aA][rR][tT]"
                  )
                 "\\>\\|\\<")
             "\\>\\)") 1 'font-lock-keyword-face)
    ;; These keywords can be anywhere on a line
    (list
     (concat "\\(\\<"
      (mapconcat 'identity
                 '(
                   "[eE][xX][iI][sS][tT]"
                   "[eE][rR][rR][oO][rR][lL][eE][vV][eE][lL]"
                   "[gG][oO][tT][oO]"
                   "[nN][oO][tT]"
                  )
                 "\\>\\|\\<")
             "\\>\\)") 1 'font-lock-keyword-face)
	(list "^[ \t]*\\(:\\sw+\\)"      1 'font-lock-function-name-face t)
	(list "\\(%\\sw+%\\)"		 1 'font-lock-reference-face)
	(list "\\(%[0-9]\\)"		 1 'font-lock-reference-face)
	(list "\\(/[^/ \t\n]+\\)"	 1 'font-lock-type-face)
	(list "\\<\\([gG][oO][tT][oO]\\)\\>[ \t]*\\(\\sw+\\)?" 
	      '(1 font-lock-keyword-face)
	      '(2 font-lock-function-name-face nil t))

  )
  "Keywords to hilight in BAT mode")

;;; don't do it in Win-Emacs
(if (boundp 'font-lock-defaults-alist)
    (setq font-lock-defaults-alist
	  (append 
	   (list (cons 'bat-mode
		       (list 'bat-font-lock-keywords nil t nil nil)))
	   font-lock-defaults-alist)))

(provide 'bat-mode)

;;; bat-mode.el ends here

