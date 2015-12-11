#!/bin/bash

n=$(mktemp -d /tmp/emacs-clean.XXXXXX)
cd $n
${EMACS:-emacs} -Q -l ~/.emacs.d/emacs-clean.el
