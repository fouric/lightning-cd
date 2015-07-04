#!/usr/bin/python

import traceback
import termbox
import os

t = termbox.Termbox()

def writeText(window, x, y, text, fg, bg):
    for i in range(len(text)):
        window.change_cell(x + i, y, ord(text[i]), fg, bg)

def main():
    # modes
    NORMAL = 0
    SEARCH = 1
    searchbuffer = ''
    mode = NORMAL

    selected = 0
    while True:
        t.clear()
        files = os.listdir('.')
        y = 0
        others = False
        for f in files:
            if mode == NORMAL:
                if y != selected:
                    fg, bg = termbox.WHITE, 0
                else:
                    fg, bg = termbox.BLACK, termbox.WHITE
            elif mode == SEARCH:
                if searchbuffer != f[:len(searchbuffer)]:
                    fg, bg = termbox.WHITE, 0
                else:
                    fg, bg = termbox.BLACK, termbox.WHITE
                    if not others:
                        selected = y
                        others = True
                    else:
                        selected = 0
            writeText(t, 0, y, f, fg, bg)
            #if os.path.isfile(files[selected]):
            y += 1

        if mode == SEARCH:
            writeText(t, 0, t.height() - 1, searchbuffer, termbox.WHITE, 0)
        t.present()

        event = t.poll_event()
        keycode = event[2]
        letter = event[1]
        if mode == NORMAL:
            if letter == 'q':
                break
            elif keycode == termbox.KEY_ESC:
                break
            elif keycode == termbox.KEY_ARROW_UP or letter == 'k':
                selected -= 1
            elif keycode == termbox.KEY_ARROW_DOWN or letter == 'j':
                selected += 1
            elif keycode == termbox.KEY_ENTER:
                if os.path.isdir(files[selected]):
                    os.chdir(files[selected])
                    selected = 0
                elif os.path.isfile(files[selected]):
                    # default non-nvim file opening action here
                    pass
            elif letter == 'u':
                os.chdir('..')
                selected = 0
            elif letter == 'b':
                # TODO: implement stack where you can just go back several times
                pass
            elif letter == 'v':
                t.close()
                os.execvp('nvim', ['nvim', os.path.realpath(files[selected])])
            elif letter == 's':
                searchbuffer = ''
                mode = SEARCH
            elif letter == 'h':
                # stands for "here" and opens a directory here
                pass
        elif mode == SEARCH:
            if letter:
                if letter in 'abcdefghijklmnopqrstuvwxyz':
                    searchbuffer = searchbuffer + letter
            elif keycode == termbox.KEY_SPACE:
                mode = NORMAL
            elif keycode == termbox.KEY_ENTER:
                if os.path.isdir(files[selected]):
                    os.chdir(files[selected])
                    selected = 0
                    mode = NORMAL
                if os.path.isfile(files[selected]):
                    t.close()
                    os.execvp('nvim', ['nvim', os.path.realpath(files[selected])])
            elif keycode == termbox.KEY_BACKSPACE2:
                searchbuffer = searchbuffer[:-1]

        if selected >= len(files):
            selected = 0
        elif selected < 0:
            selected = len(files) - 1

    t.close()

try:
    main()
except Exception, e:
    f = open('error.txt', 'w')
    f.write(traceback.format_exc() + '\n')
    f.close()
    print traceback.format_exc()
