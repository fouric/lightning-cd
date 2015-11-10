# lightning-cd

[![asciicast](https://asciinema.org/a/29694.png)](https://asciinema.org/a/29694)

Lightning is a tool designed to allow you to find and open files as fast as physically possible.  It acts as a complement to autojump (https://github.com/wting/autojump); while autojump allows you to go from anywhere to your most accessed directories with just a few keystrokes, Lightning allows you to fly around your filesystem with reckless abandon, flitting through directories and opening files with ease.

Dependencies
-----------

 - Termbox (https://github.com/nsf/termbox) with Python 3 bindings installed
 - A relatively recent version of Python 3

Installation
------------

Merely copy lightning-cd.py to someplace convenient and add an alias to it, like so.  Lightning takes as its first argument the full path of a file that it writes a directory path to when quitting.  If your alias is set up like the below, then you should be able to use Lightning to change your shell's current working directory:

alias i='python3 ~/code/lightning-cd/lightning-cd.py ~/.lightningpath && cd "\`cat ~/.lightningpath\`"'

Usage
-----

Lightning is an interactive, modal tool with letter keybindings very similar to vim.  It contains two modes, search and normal.  Normal mode allows you to select a file by moving up and down through a list.  Search mode, the default, takes letters that you type, filters them, and then uses them to open a file once the search buffer contains enough to uniquely identify a file.

Common keybindings:
 - Comma moves up one directory
 - Space toggles the mode between search and normal
 - Semicolon quits Lightning
 - Single quote "does the right thing" on either the selected file (normal mode) or the first matching file (search mode)
 - Double quote refreshes the file list
 - Question mark toggles the visibility of "hidden" files (defined by those matching hiddenFilesPattern in settings.py)

Search keybindings:
 - Letters are converted to lowercase, and along with period and numbers are valid search characters
 - Dash removes a character from the search buffer

Normal keybindings:
 - j and k move down and up, respectively
 - f opens the current directory with your file browser (Nautilus by default)
 - t opens Tmux at the current directory

All of the above keybindings can (and should) be changed by editing keybindings.py.

Settings in settings.py:
 - showDeselectedFiles controls the visibility of filenames that don't match the search buffer
 - defaultMode sets the default mode for startup and mode switches
 - fileBrowser is the file browser opened when the file browser shortcut ('f' by default) is pressed
 - persistentMode, if set, causes the current mode to stay the same on directory change
 - lightningPathFile is the path of the file that Lightning writes the directory that should be cd'ed to
 - showHiddenFiles sets the default visibility of hidden files
 - hiddenFilesPattern is the regexp (Python3 re module) that, if matched, declares a pathname (works for files and directories) to be "hidden"
 - restrictBuffer, if set, will cause the search buffer to refuse to accept characters that don't match at least one existing pathname
 - mimePatterns tells Lightning, using mimetypes, how to handle files when told to open them

One of the basic assumptions behind Lightning is that you'll be spending most of your time in search mode, and only switching to normal mode to select files that would otherwise require a (relatively) large number of keystrokes. You may violate this assumption if you wish, at which point you will discover that you are using a watered-down `mc` (http://www.midnight-commander.org/). Play off Lightning's strengths, avoid its weaknesses (or use another tool), and you may find yourself enjoying productivity gains due to lower mental costs associated with switching source code files.

Disclaimer
---------

Lightning is ALPHA, very much buggy and non-feature-complete, and likely to change very quickly (with changes possibly including *a move to another programming language* - use at your own risk).  With that said, the basic idea (of finding and opening files as quickly as physically possible) will remain the same, and you are encouraged to try it out and give feedback.

(it was also built for a Dvorak Simplified Keyboard user, so you may want to tweak the keybindings for convenience and comfort if you use QWERTY)
