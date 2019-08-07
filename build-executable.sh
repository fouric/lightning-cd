#!/bin/sh

# requires xach's buildapp (https://www.xach.com/lisp/buildapp/)

# replace ~/quicklisp with the path to your quicklisp install
buildapp --output lightning-cd --asdf-tree ~/quicklisp --load-system lightning-cd --entry lightning-cd:main
