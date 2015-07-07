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
def selectFilesOnsearchBuffer(files, searchBuffer):# {{{
    selected = []
    for f in files:
        if searchBuffer == filenameClean(f)[:len(searchBuffer)]:
            selected.append(f)
    return selected
# }}}
def getFileIndex(f, files):# {{{
    for i in range(len(files)):
        if f == files[i]:
            return i# }}}
def drawFileList(t, ystart, yend, mode, selected, selectedFiles):# {{{
    y = ystart
    for f in files:
        if y == yend:
            break
        if mode == SEARCH:
            if f in selectedFiles:
                fg, bg = termbox.BLACK, termbox.WHITE
            else:
                fg, bg = termbox.WHITE, 0
        elif mode == NORMAL:
            if f == files[selected]:
                fg, bg = termbox.BLACK, termbox.WHITE
            else:
                fg, bg = termbox.WHITE, 0
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
def relativeSelect(f, relative):# {{{
    index = getFileIndex(files, f)
    index += relative
    index = mod(index, files)
    return [files[index]]
# }}}
def switchMode(prevMode, selected, selectedFiles):# {{{
    if prevMode == SEARCH:
        selected = 0
        if len(selectedFiles):
            selected = getFileIndex(selectedFiles[0], files)
        newMode = NORMAL
    else:
        newMode = SEARCH
    searchBuffer = ''
    selectedFiles = []
    return (newMode, selected, searchBuffer, selectedFiles)
# }}}
def action(f, originalPath):# {{{
    if os.path.isdir(f):
        os.chdir(f)
        selected = 0
        mode = defaultMode
        selectedFiles = []
        searchBuffer = ''
        return (mode, selected, selectedFiles, searchBuffer)
    elif os.path.isfile(f):
        command(originalPath, 'nvim ' + f)
# }}}
def command(path, command):# {{{
    t.close()
    print path
    os.system(command)
    quit()# }}}

try:# {{{
    originalPath = os.path.realpath('.')
    NORMAL, SEARCH = 0, 1
    defaultMode = SEARCH
    mode = defaultMode
    selectedFiles = []
    searchBuffer = ''
    selected = 0
    files = []

    t = termbox.Termbox()
    while True:
        t.clear()
        files = os.listdir('.')
        if mode == SEARCH:
            selectedFiles = selectFilesOnsearchBuffer(files, searchBuffer)
        drawFileList(t, 1, t.height() - 1, mode, selected, selectedFiles)
        if mode == SEARCH:
            if len(selectedFiles) == 1 and len(searchBuffer):
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], originalPath)
                continue
            writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
        writeText(t, 0, 0, os.path.realpath('.'), termbox.WHITE, 0)
        t.present()

        event = t.poll_event()
        letter, keycode = event[1], event[2]
        if letter == '.':
            # this definitely needs to be changed; '.' is wayyy to common in filenames
            os.chdir('..')
            searchBuffer = ''
            selected = 0
        elif keycode == termbox.KEY_SPACE:
            mode, selected, searchBuffer, selectedFiles = switchMode(mode, selected, selectedFiles)
        elif keycode == termbox.KEY_ESC:
            break
        elif mode == NORMAL:
            if keycode == termbox.KEY_ARROW_UP or letter == 'k':
                selected = mod(selected - 1, files)
            elif keycode == termbox.KEY_ARROW_DOWN or letter == 'j':
                selected = mod(selected + 1, files)
            elif keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(files[selected], originalPath)
            elif letter == 'q':
                break
            elif letter == 'v':
                command(originalPath, 'nvim ' + files[selected])
            elif letter == 'h':
                command(os.path.realpath('.'), 'true')
            elif letter == 'n':
                command(originalPath, 'nautilus ' + os.path.realpath('.'))
            elif letter == 't':
                command(originalPath, 'tmux')
        elif mode == SEARCH:
            if letter:
                if letter in getCharRange():
                    searchBuffer = searchBuffer + letter
            if keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], originalPath)
            elif keycode == termbox.KEY_BACKSPACE2:
                searchBuffer = searchBuffer[:-1]

    t.close()
    print originalPath# }}}
except Exception, e:# {{{
    #f = open('~/lightning-cd-error.txt', 'w')
    f = open('error.txt', 'w')
    f.write(traceback.format_exc() + '\n')
    f.close()
    try:
        tbox.close()
    except:
        pass
    print traceback.format_exc()# }}}
