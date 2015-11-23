# lightning-cd

[![asciicast](https://asciinema.org/a/29694.png)](https://asciinema.org/a/29694)

Lightning is a tool designed to allow you to find and open files as fast as physically possible.  It acts as a complement to autojump (https://github.com/wting/autojump); while autojump allows you to go from anywhere to your most accessed directories with just a few keystrokes, Lightning allows you to fly around your filesystem with reckless abandon, flitting through directories and opening files with ease.


Disclaimer
---------

This is the Common Lisp branch of Lightning, and is currently not quite ready for production use just yet. The original version is written in Python, and is much more fast, stable, and powerful than the Lisp version; if you want to use Lightning, then you're probably better off using the Python branch until things get just a bit more stable here. However, I plan to make this branch the primary one as soon as it works reasonably well, so you might want to be prepared to migrate sometime in the near future.

Dependencies
-----------

 - [Termbox](https://github.com/nsf/termbox)
 - A modern Common Lisp implementation with support for [ASDF](https://common-lisp.net/project/asdf/asdf.html) ([SBCL](http://www.sbcl.org/) is a good choice)
 - [cl-termbox](https://github.com/fouric/cl-termbox)

Installation
------------

Copy the Lightning directory to someplace convenient, build a binary with your favorite Common Lisp compiler (instructions coming for SBCL soon, although it isn't too hard) then add an alias to it, like so:

    alias i='~/code/lightning-cd/lightning-cd.elf && cd "`cat ~/.lightningpath`" && bash -c "`cat ~/.lightningcommand`"'

Because I currently can't find an easy way to exec directly from Lisp to an external command, or to change the invoking shell's current working directory, Lightning writes to two files: `.lightningpath`, which contains a path to `cd` to, and `.lightningcommand`, which contains a command to invoke (usually your text editor, although Lightning will soon be able to invoke other utilities).

Usage
-----

Lightning is an interactive, modal tool with letter keybindings very similar to vim.  It contains two modes, search and normal.  Normal mode allows you to select a file by moving up and down through a list.  Search mode, the default, takes letters that you type, filters them, and then uses them to open a file once the search buffer contains enough to uniquely identify a file.

Actual keybindings and settings will be HERE as soon as they work.

One of the basic assumptions behind Lightning is that you'll be spending most of your time in search mode, and only switching to normal mode to select files that would otherwise require a (relatively) large number of keystrokes. You may violate this assumption if you wish, at which point you will discover that you are using a watered-down `mc` (http://www.midnight-commander.org/). Play off Lightning's strengths, avoid its weaknesses (or use another tool), and you may find yourself enjoying productivity gains due to lower mental costs associated with switching source code files.
