# lightning-cd

[![asciicast](https://asciinema.org/a/29694.png)](https://asciinema.org/a/29694)

Lightning is a tool designed to allow you to find and open files as fast as physically possible.  It acts as a complement to autojump (https://github.com/wting/autojump); while autojump allows you to go from anywhere to your most accessed directories with just a few keystrokes, Lightning allows you to fly around your filesystem with reckless abandon, flitting through directories and opening files with ease.


Disclaimer
---------

This is the Common Lisp branch of Lightning, and is currently virtually unusable. The original version is written in Python, and is _much_ more fast, stable, and powerful than the Lisp version; if you want to actually _use_ Lightning, then you should use the Python branch, as I do. However, I plan to make this branch the primary one as soon as it works reasonably well, so you might want to be prepared to migrate sometime in the near future.

Dependencies
-----------

 - [Termbox](https://github.com/nsf/termbox)
 - A modern Common Lisp implementation with support for (ASDF)[https://common-lisp.net/project/asdf/asdf.html] ([SBCL](http://www.sbcl.org/) is a good choice)
 - [cl-termbox](https://github.com/fouric/cl-termbox)

Installation
------------

Copy the Lightning directory to someplace convenient, then add an alias to it, like so:

alias i='sbcl ~/code/lightning-cd/lightning-cd.lisp ~/.lightningpath && cd "\`cat ~/.lightningpath\`"'

Lightning takes as its primary argument the path of a file to write a directory path to. While quitting, Lightning will write the current working directory to that file, and if you have an alias set up similar to the above, then your shell should move to the directory where Lightning quit.

Usage
-----

Lightning is an interactive, modal tool with letter keybindings very similar to vim.  It contains two modes, search and normal.  Normal mode allows you to select a file by moving up and down through a list.  Search mode, the default, takes letters that you type, filters them, and then uses them to open a file once the search buffer contains enough to uniquely identify a file.

Actual keybindings and settings will be HERE as soon as they work.

One of the basic assumptions behind Lightning is that you'll be spending most of your time in search mode, and only switching to normal mode to select files that would otherwise require a (relatively) large number of keystrokes. You may violate this assumption if you wish, at which point you will discover that you are using a watered-down `mc` (http://www.midnight-commander.org/). Play off Lightning's strengths, avoid its weaknesses (or use another tool), and you may find yourself enjoying productivity gains due to lower mental costs associated with switching source code files.
