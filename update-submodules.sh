#!/bin/bash

git submodule foreach 'git pull; git ribbon-catchup; git ribbon-set'
