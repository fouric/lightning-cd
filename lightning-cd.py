#!/usr/bin/python

import traceback
import termbox
import os
from settings import *
import keybindings


showDeselectedFiles = False
Mode = Enum('Mode', 'NORMAL SEARCH')
defaultMode = Mode.SEARCH
editor = 'nvim'
fileBrowser = 'nautilus'
persistentMode = True
lightningPathFile = os.sys.argv[1]

def writeText(t, x, y, text, fg, bg):
    "Execute a series of change_cell's in a sequential manner such as to write a line of text"
    for i in range(len(text)):
        t.change_cell(x + i, y, ord(text[i]), fg, bg)

def getCharRange():
    "Get a string of the characters that are valid to enter in search mode"
    chars = ''
    for i in range(255):
        if (i >= ord('a') and i <= ord('z')) or (i >= ord('0') and i <= ord('9')):
            chars = chars + chr(i)
    chars = chars + '.'
    return chars

def filenameClean(filename):
    "Convert a raw filename to a simplified one that can be searched for"
    newFilename = ''
    filename = filename.lower()
    acceptableChars = getCharRange()
    for char in filename:
        if char in acceptableChars:
            newFilename = newFilename + char
    return newFilename

def selectFilesOnsearchBuffer(files, searchBuffer):
    "Return a list of selected files by comparing simplified filenames with the search buffer"
    selected = []
    for f in files:
        if searchBuffer == filenameClean(f)[:len(searchBuffer)]:
            selected.append(f)
    return selected

def getFileColors(mode, selectedFiles, thisFile, fileList):
    if mode == Mode.SEARCH and thisFile in selectedFiles and showDeselectedFiles:
        fg, bg = termbox.BLACK, termbox.WHITE
    elif mode == Mode.NORMAL and thisFile == fileList[selected]:
        fg, bg = termbox.BLACK, termbox.WHITE
    else:
        fg, bg = termbox.WHITE, 0
    return (fg, bg)

def showThisFile(thisFile, selectedFiles):
    return showDeselectedFiles or thisFile in selectedFiles or selectedFiles == []

def writePath(filename, path):
    f = open(filename, 'w+')
    f.write(path)
    f.close()

def drawFileList(t, ystart, yend, mode, selected, selectedFiles):
    "Draw the list of selected files onto the screen"
    x = 0
    width = 1
    y = ystart
    for f in files:
        # if we've reached the end of a column then begin at the top of the next one
        if y == yend:
            y = ystart
            x += width
            width = 1
        # get the foreground and background colors for a particular filename
        fg, bg = getFileColors(mode, selectedFiles, f, files)
        if showThisFile(f, selectedFiles):
            if os.path.isdir(f):
                f = f + '/'
            width = max(width, len(f) + 1)
            writeText(t, x, y, f, fg, bg)
            y += 1

def switchMode(prevMode, selected, selectedFiles):
    "Switch the mode to either search or normal and do associated setup for each mode"
    if prevMode == Mode.SEARCH:
        selected = 0
        if len(selectedFiles):
            selected = files.index(selectedFiles[0])
        newMode = Mode.NORMAL
    else:
        newMode = Mode.SEARCH
    searchBuffer = ''
    selectedFiles = []
    return (newMode, selected, searchBuffer, selectedFiles)

def takeActionOnPath(f, path):
    "Do something with a filename that the user selected"
    if os.path.isdir(f):
        os.chdir(f)
        selected = 0
        if not persistentMode:
            newMode = defaultMode
        else:
            newMode = mode
        selectedFiles = []
        searchBuffer = ''
        return (newMode, selected, selectedFiles, searchBuffer)
    elif os.path.isfile(f):
        runCommandOnFile(path, editor + ' ' + f)

def runCommandOnFile(path, command):
    "Close lightning, write the current path, and execute the command"
    t.close()
    writePath(lightningPathFile, path)
    os.system(command)
    quit()

if __name__ == '__main__':
    try:
        mode = defaultMode
        selectedFiles = []
        searchBuffer = ''
        selected = 0
        files = []
        charRange = getCharRange()
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
            if mode == Mode.SEARCH:
                selectedFiles = selectFilesOnsearchBuffer(files, searchBuffer)
            drawFileList(t, 1, t.height() - 1, mode, selected, selectedFiles)
            if mode == Mode.SEARCH:
                if len(selectedFiles) == 1 and len(searchBuffer):
                    mode, selected, selectedFiles, searchBuffer = takeActionOnPath(selectedFiles[0], os.path.realpath('.'))
                    continue
                writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
            if mode == Mode.SEARCH:
                modeText = "search"
            elif mode == Mode.NORMAL:
                modeText = "normal"
            writeText(t, 0, 0, modeText + ": ", termbox.WHITE, 0)
            writeText(t, len(modeText) + 2, 0, os.path.realpath('.'), termbox.WHITE, 0)
            t.present()
            event = t.poll_event()
            letter, keycode = event[1], event[2]
            if keycode == termbox.KEY_SPACE:
                mode, selected, searchBuffer, selectedFiles = switchMode(mode, selected, selectedFiles)
            elif letter == keybindings.KEY_UP_DIR:
                os.chdir('..')
                searchBuffer = ''
                if not persistentMode:
                    mode = defaultMode
                selectedFiles = []
                selected = 0
            elif letter == keybindings.KEY_QUIT:
                runCommandOnFile(os.path.realpath('.'), 'true')
            elif letter == keybindings.KEY_SMART:
                mode, selected, selectedFiles, searchBuffer = takeActionOnPath(files[0 if mode == Mode.SEARCH else selected], os.path.realpath('.'))
            elif mode == Mode.NORMAL:
                if letter == keybindings.KEY_UP:
                    selected = (selected - 1) % len(files)
                elif letter == keybindings.KEY_DOWN:
                    selected = (selected + 1) % len(files)
                elif letter == keybindings.KEY_EDITOR:
                    runCommandOnFile(os.path.realpath('.'), editor + ' ' + files[selected])
                elif letter == keybindings.KEY_FILE_BROWSER:
                    runCommandOnFile(os.path.realpath('.'), fileBrowser + ' ' + os.path.realpath('.') + ' > /dev/null 2>&1')
                elif letter == keybindings.KEY_TMUX:
                    runCommandOnFile(os.path.realpath('.'), 'tmux > /dev/null')
            elif mode == Mode.SEARCH:
                if letter:
                    if letter in charRange:
                        searchBuffer = searchBuffer + letter
                    elif letter == keybindings.KEY_DELETE:
                        searchBuffer = searchBuffer[:-1]
        t.close()
        print os.path.realpath('.')
    except Exception, e:
        f = open('error.txt', 'w')
        f.write(traceback.format_exc() + '\n')
        f.close()
        try:
            t.close()
        except:
            pass
        print traceback.format_exc()
