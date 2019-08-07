#!/usr/bin/fish

# https://superuser.com/questions/361902/continuously-re-execute-a-command-when-it-finishes-in-bash

# reminder: check your color scheme if you're getting gray backgrounds!

e src/lightning.lisp
/usr/bin/sbcl --noinform --load lightning-dev.lisp
