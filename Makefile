all:
	(cd site-lisp/helm && find . -name '*.elc' -delete && make)
	(cd site-lisp/org-mode && find . -name '*.elc' -delete && make)
