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
def drawFileList(t, y, mode, selected, selectedFiles):# {{{
    for f in files:
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
def switchMode(newMode, prevMode, selected, selectedFiles):# {{{
    if newMode == NORMAL and prevMode == SEARCH:
        selected = 0
        if len(selectedFiles):
            selected = getFileIndex(selectedFiles[0], files)
    searchBuffer = ''
    selectedFiles = []
    return (newMode, selected, searchBuffer, selectedFiles)
# }}}
def action(f):# {{{
    if os.path.isdir(f):
        os.chdir(f)
        selected = 0
        mode = defaultMode
        selectedFiles = []
        searchBuffer = ''
        return (mode, selected, selectedFiles, searchBuffer)
    elif os.path.isfile(f):
        t.close()
        os.execvp('nvim', ['nvim', os.path.realpath(f)])
# }}}

try:# {{{
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
        drawFileList(t, 0, mode, selected, selectedFiles)
        if mode == SEARCH:
            if len(selectedFiles) == 1 and len(searchBuffer):
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0])
                continue
            writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
        t.present()

        event = t.poll_event()
        letter, keycode = event[1], event[2]
        if mode == NORMAL:
            if letter == 'q' or keycode == termbox.KEY_ESC:
                break
            elif keycode == termbox.KEY_ARROW_UP or letter == 'k':
                selected = mod(selected - 1, files)
            elif keycode == termbox.KEY_ARROW_DOWN or letter == 'j':
                selected = mod(selected + 1, files)
            elif keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0])
            elif letter == '.':
                os.chdir('..')
                selected = 0
            elif letter == 'b':
                # TODO: implement stack where you can just go back several times
                pass
            elif letter == 'v':
                t.close()
                os.execvp('nvim', ['nvim', os.path.realpath(files[selected])])
            elif keycode == termbox.KEY_SPACE:
                mode, selected, searchBuffer, selectedFiles = switchMode(SEARCH, NORMAL, selected, selectedFiles)
            elif letter == 'h':
                t.close()
                print os.path.realpath('.')
                quit()
        elif mode == SEARCH:
            if letter:
                if letter in getCharRange():
                    searchBuffer = searchBuffer + letter
                elif letter == '.':
                    os.chdir('..')
                    searchBuffer = ''
            if keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0])
            elif keycode == termbox.KEY_BACKSPACE2:
                searchBuffer = searchBuffer[:-1]
            elif keycode == termbox.KEY_ESC or keycode == termbox.KEY_SPACE:
                mode, selected, searchBuffer, selectedFiles = switchMode(NORMAL, SEARCH, selected, selectedFiles)

    t.close()
    print '.'# }}}
except Exception, e:# {{{
    f = open('error.txt', 'w')
    f.write(traceback.format_exc() + '\n')
    f.close()
    try:
        tbox.close()
    except:
        pass
    print traceback.format_exc()# }}}
