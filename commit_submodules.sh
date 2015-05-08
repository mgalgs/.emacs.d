#!/bin/bash

(echo -e "submodules: new commits\n"; git submodule-changelog) | git commit -aF -
