# lightning-cd

Lightning is a file browser and text editor launcher designed to allow you to find and edit files as fast as physically possible.  It acts as a complement to autojump; if autojump is the O(1) scroll of town teleport in constant time, then Lightning is the O(n) blink in linear time.

Dependencies
-----------

 - Termbox (https://github.com/nsf/termbox) with Python 3 bindings installed
 - A relatively recent version of Python 3

Installation
------------

Merely copy lightning-cd.py to someplace convenient and add an alias to it, like so.  Lightning takes as its first argument the full path of a file that it writes a directory path to when quitting.  If your alias is set up like the below, then you should be able to use Lightning to change your shell's current working directory:

alias li='python3 ~/code/lightning-cd/lightning-cd.py ~/.lightningpath && cd "\`cat ~/.lightningpath\`"'

Usage
-----

Lightning is an interactive, modal tool with letter keybindings very similar to vim.  It contains two modes, search and normal.  Normal mode allows you to select a file by moving up and down through a list.  Search mode, the default, takes letters that you type, filters them, and then uses them to open a file once the search buffer contains enough to uniquely identify a file.

Common keybindings:
 - Comma moves up one directory
 - Space toggles the mode between search and normal
 - Semicolon quits Lightning
 - Single quote "does the right thing" on either the selected file or the first matching file
 - Double quote refreshes the file list

Search keybindings:
 - Letters are converted to lowercase, and along with period and numbers are valid search characters
 - Dash removes a character from the search buffer

Normal keybindings:
 - j and k move down and up, respectively
 - v opens the currently selected file with your text editor (NeoVim by default)
 - f opens the current directory with your file browser (Nautilus by default)
 - t opens Tmux at the current directory
 - d toggles the visibility of "hidden" files (currently those whose filenames start with a dot)

Disclaimer
---------

Lightning is ALPHA, very much buggy and non-feature-complete, and likely to change very quickly.  With that said, the basic idea (of finding and editing files as quickly as physically possible) will remain the same, and you are encouraged to try it out and give feedback.

(it was also built for a Dvorak Simplified Keyboard user, so you may want to tweak it if you use QWERTY)
