#!/bin/sh

/usr/bin/sbcl --noinform --eval '(progn (ql:quickload :lightning-cd) (funcall (intern "LIGHTNING-CD" :lightning-cd)))'
