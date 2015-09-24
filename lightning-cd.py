#!/usr/bin/python

import traceback
import termbox
import os

showDeselectedFiles = False

def writeText(t, x, y, text, fg, bg):# {{{
    "Execute a series of change_cell's in a sequential manner such as to write a line of text"
    for i in range(len(text)):
        t.change_cell(x + i, y, ord(text[i]), fg, bg)
# }}}
def getCharRange():# {{{
    "Get a string of the characters that are valid to enter in search mode"
    chars = ''
    for i in range(255):
        if (i >= ord('a') and i <= ord('z')) or (i >= ord('0') and i <= ord('9')):
            chars = chars + chr(i)
    chars = chars + '.'
    return chars
# }}}
def filenameClean(filename):# {{{
    "Convert a raw filename to a simplified one that can be searched for"
    newFilename = ''
    filename = filename.lower()
    acceptableChars = getCharRange()
    for char in filename:
        if char in acceptableChars:
            newFilename = newFilename + char
    return newFilename
# }}}
def selectFilesOnsearchBuffer(files, searchBuffer):# {{{
    "Return a list of selected files by comparing simplified filenames with the search buffer"
    selected = []
    for f in files:
        if searchBuffer == filenameClean(f)[:len(searchBuffer)]:
            selected.append(f)
    return selected
# }}}
def drawFileList(t, ystart, yend, mode, selected, selectedFiles):# {{{
    "Draw the list of selected files onto the screen"
    y = ystart
    for f in files:
        if y == yend:
            break
        elif mode == SEARCH and f in selectedFiles and showDeselectedFiles:
            fg, bg = termbox.BLACK, termbox.WHITE
        elif mode == NORMAL and f == files[selected]:
            fg, bg = termbox.BLACK, termbox.WHITE
        else:
            fg, bg = termbox.WHITE, 0
        if showDeselectedFiles or f in selectedFiles or selectedFiles == []:
            if os.path.isdir(f):
                f = f + '/'
            writeText(t, 0, y, f, fg, bg)
            y += 1 # }}}
def switchMode(prevMode, selected, selectedFiles):# {{{
    "Switch the mode to either search or normal and do associated setup for each mode"
    if prevMode == SEARCH:
        selected = 0
        if len(selectedFiles):
            selected = files.index(selectedFiles[0])
        newMode = NORMAL
    else:
        newMode = SEARCH
    searchBuffer = ''
    selectedFiles = []
    return (newMode, selected, searchBuffer, selectedFiles)
# }}}
def action(f, originalPath):# {{{
    "Do something with a filename that the user selected"
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
    "Close lightning, print the current path, and execute the command"
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
        files = sorted(os.listdir('.'))
        normalfiles = []
        dotfiles = []
        for f in files:
            if f[0] == '.':
                dotfiles.append(f)
            else:
                normalfiles.append(f)
        files = normalfiles + dotfiles
        if mode == SEARCH:
            selectedFiles = selectFilesOnsearchBuffer(files, searchBuffer)
        drawFileList(t, 1, t.height() - 1, mode, selected, selectedFiles)
        if mode == SEARCH:
            if len(selectedFiles) == 1 and len(searchBuffer):
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], os.path.realpath('.'))
                continue
            writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
        writeText(t, 0, 0, os.path.realpath('.'), termbox.WHITE, 0)
        t.present()

        event = t.poll_event()
        letter, keycode = event[1], event[2]
        if letter == ',':
            os.chdir('..')
            searchBuffer = ''
            selected = 0
        elif keycode == termbox.KEY_SPACE:
            mode, selected, searchBuffer, selectedFiles = switchMode(mode, selected, selectedFiles)
        elif keycode == termbox.KEY_ESC:
            break
        elif mode == NORMAL:
            if letter == 'k':
                selected = selected - 1 % len(files)
            elif letter == 'j':
                selected = selected + 1 % len(files)
            elif letter == '\'':
                mode, selected, selectedFiles, searchBuffer = action(files[selected], originalPath)
            elif letter == 'q':
                break
            elif letter == 'v':
                command(originalPath, 'nvim ' + files[selected])
            elif letter == 'h':
                command(os.path.realpath('.'), 'true')
            elif letter == 'n':
                command(originalPath, 'nautilus ' + os.path.realpath('.') + ' > /dev/null 2>&1')
            elif letter == 't':
                command(originalPath, 'tmux > /dev/null')
            elif letter == 'T':
                command(os.path.realpath('.'), 'tmux > /dev/null')
        elif mode == SEARCH:
            if letter:
                if letter in getCharRange():
                    searchBuffer = searchBuffer + letter
                elif letter == '-':
                    searchBuffer = searchBuffer[:-1]
            if keycode == termbox.KEY_ENTER:
                mode, selected, selectedFiles, searchBuffer = action(selectedFiles[0], originalPath)

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
