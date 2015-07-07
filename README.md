# lightning-cd

lightning-cd (or, more colloquially, Lightning) is a file browser and text editor launcher designed to allow you to find and edit files as fast as physically possible.  It acts as a complement to autojump; if autojump is the O(1) scroll of town teleport in constant time, then Lightning is the O(n) blink in linear time.

Dependencies
-----------

Termbox (https://github.com/nsf/termbox).  That's it.

Installation
------------

Merely copy lightning.py to someplace convenient and add an alias to it, like so.  Lightning returns a directory if you use the "h" (Here) in normal mode, so an alias such as

alias li='cd `python ~/code/lightning-cd/lightning-cd.py`'

will allow you to jump into directories without opening files.

Usage
-----

Lightning has two modes: search and normal.  By default, it starts in search mode, and it defaults to search every time you change a directory.  Press space in either mode to toggle between modes, and . (period) in either mode to move one directory upward.  In search mode, typing any lowercase letter will begin to filter files and directories, and after you have typed enough to uniquely identify a file or directory, Lightning will automatically either enter that directory or open that file in NeoVim, depending on whether you have selected a file or directory.  In normal mode, "q" quits, "h" opens up the current directory in your shell, provided that you have the correct alias set, "j" and "k" go down and up, respectively, and "enter" opens the file or directory.

Disclaimer
---------

Lightning is ALPHA, very much buggy and non-feature-complete, and likely to change very quickly.  With that said, the basic idea (of finding and editing files as quickly as physically possible) will remain the same, and you are encouraged to try it out and give feedback.
