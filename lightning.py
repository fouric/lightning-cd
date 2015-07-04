#!/usr/bin/python

import traceback
import termbox
import os

def writeText(t, x, y, text, fg, bg):# {{{
    for i in range(len(text)):
        t.change_cell(x + i, y, ord(text[i]), fg, bg)
# }}}
def getCharRange():# {{{
    chars = ''
    for i in range(255):
        if i >= ord('a') and i <= ord('z'):
            chars = chars + chr(i)
    return chars
# }}}
def filenameClean(filename):# {{{
    newFilename = ''
    filename = filename.lower()
    acceptableChars = getCharRange()
    for char in filename:
        if char in acceptableChars:
            newFilename = newFilename + char
    return newFilename
# }}}
def selectFilesOnSearchbuffer(files, searchbuffer):# {{{
    selected = []
    for f in files:
        if searchbuffer == filenameClean(f)[:len(searchbuffer)]:
            selected.append(f)
    return selected
# }}}
def getFileIndex(files, f):# {{{
    for i in range(len(files)):
        if f == files[i]:
            return i# }}}
def drawFileList(t, y, files, selectedFiles):# {{{
    for f in files:
        if f not in selectedFiles:
            fg, bg = termbox.WHITE, 0
        else:
            fg, bg = termbox.BLACK, termbox.WHITE
        if os.path.isdir(f):
            f = f + '/'
        writeText(t, 0, y, f, fg, bg)
        y += 1# }}}
def mod(i, files):# {{{
    if i >= len(files):
        i = 0
    elif i < 0:
        i = len(files) - 1
    return i
# }}}
def relativeSelect(files, f, relative):# {{{
    index = getFileIndex(files, f)
    index += relative
    index = mod(index, files)
    return [files[index]]
# }}}
def main(t):
    # modes
    NORMAL = 0
    SEARCH = 1
    mode = NORMAL

    searchbuffer = ''
    selectedFiles = []
    selected = 0

    while True:
        t.clear()
        files = os.listdir('.')
        if mode == NORMAL:
            if len(selectedFiles):
                selected = getFileIndex(selectedFiles[0], files)
                selectedFiles = []
                selected = 0
            drawFileList(t, 0, files, [files[selected]])
        elif mode == SEARCH:
            selectedFiles = selectFilesOnSearchbuffer(files, searchbuffer)
            writeText(t, 0, t.height() - 1, searchbuffer, termbox.WHITE, 0)
            drawFileList(t, 0, files, selectedFiles)
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
                selected = mod(selected - 1, files)
            elif keycode == termbox.KEY_ARROW_DOWN or letter == 'j':
                selected = mod(selected + 1, files)
            elif keycode == termbox.KEY_ENTER:
                if os.path.isdir(files[selected]):
                    os.chdir(files[selected])
                    selected = 0
                elif os.path.isfile(files[selected]):
                    # possibly allow for other actions such as make?
                    t.close()
                    os.execvp('nvim', ['nvim', os.path.realpath(files[selected])])
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
                return os.path.realpath('.')
        elif mode == SEARCH:
            if letter:
                if letter in getCharRange():
                    searchbuffer = searchbuffer + letter
            elif keycode == termbox.KEY_SPACE:
                mode = NORMAL
            elif keycode == termbox.KEY_ENTER:
                if len(selectedFiles):
                    if os.path.isdir(selectedFiles[0]):
                        os.chdir(selectedFiles[0])
                        selected = 0
                        mode = NORMAL
                        selectedFiles = []
                    elif os.path.isfile(selectedFiles[0]):
                        t.close()
                        os.execvp('nvim', ['nvim', os.path.realpath(selectedFiles[0])])
            elif keycode == termbox.KEY_BACKSPACE2:
                searchbuffer = searchbuffer[:-1]

    t.close()
    return '.'

try:# {{{
    tbox = termbox.Termbox()
    print main(tbox)
except Exception, e:
    f = open('error.txt', 'w')
    f.write(traceback.format_exc() + '\n')
    f.close()
    try:
        tbox.close()
    except:
        pass
    print traceback.format_exc()# }}}
