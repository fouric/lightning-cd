# lightning-cd

lightning-cd (or, more colloquially, Lightning) is a file browser and text editor launcher designed to allow you to find and edit files as fast as physically possible.  It acts as a complement to autojump; if autojump is the O(1) scroll of town teleport in constant time, then Lightning is the O(n) blink in linear time.

Dependencies
-----------

Termbox (https://github.com/nsf/termbox)

Installation
------------

Merely copy lightning.py to someplace convenient and add an alias to it, like so.  Lightning takes as its first argument the full path of a file that it writes a directory path to when quitting.  If your alias is set up like the below, then you should be able to use Lightning to change your shell's current working directory:

alias li='python ~/code/lightning-cd/lightning-cd.py ~/.lightningpath && cd `cat ~/.lightningpath`'

Usage
-----

Lightning is an interactive, modal tool with letter keybindings very similar to vim.  It contains two modes, search and normal.  Normal mode allows you to select a file by moving up and down through a list.  Search mode, the default, takes letters that you type, filters them, and then uses them to open a file once the search buffer contains enough to uniquely identify a file.

Common keybindings:
 - Escape quits Lightning
 - Comma moves up one directory, preserving the current mode
 - Space toggles the mode between search and normal
 - Semicolon quits Lightning, changing to the current directory

Search keybindings:
 - Letters are converted to lowercase, and along with period are valid search characters
 - Enter opens the first file that matches the search buffer
 - Dash removes a character from the search buffer

Normal keybindings:
 - j and k move down and up, respectively
 - q quits
 - ' "does the right thing" on the current selection
 - v uses Neovim to open the current selection
 - n opens Nautilus at the current location and quits Lightning
 - t opens prints out Lightning's initial directory, opens tmux at the current location, and quits Lightning
 - T opens prints out Lightning's current directory, opens tmux at the current location, and quits Lightning

Disclaimer
---------

Lightning is ALPHA, very much buggy and non-feature-complete, and likely to change very quickly.  With that said, the basic idea (of finding and editing files as quickly as physically possible) will remain the same, and you are encouraged to try it out and give feedback.

(it was also built for a Dvorak Simplified Keyboard user, so you may have to tweak it if you use QWERTY)
