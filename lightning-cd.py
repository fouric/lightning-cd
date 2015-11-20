#!/usr/bin/python3

import traceback
import termbox
import os
import sys
import re
import subprocess

#sys.path.append(os.path.dirname(os.path.abspath(__file__)))
sys.path.append('/home/grant/Ramdisk/lcache/termbox')
from settings import *
import keybindings

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

def getFileColors(mode, selected, thisFile, fileList):
    if mode == Mode.NORMAL and thisFile == fileList[selected]:
        fg, bg = termbox.BLACK, termbox.WHITE
    else:
        fg, bg = termbox.BLUE if os.path.isdir(thisFile) else termbox.WHITE, 0
    return (fg, bg)

def showThisFile(thisFile, mode, selected):
    return mode == Mode.NORMAL or thisFile in selected or selected == []

def writePath(filename, path):
    f = open(filename, 'w+')
    f.write(path)
    f.close()

def selectedValueForMode(mode):
    return [] if mode == Mode.SEARCH else 0

def drawFileList(t, ystart, yend, xend, mode, selected):
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
        if x >= xend:
            break
        # get the foreground and background colors for a particular filename
        fg, bg = getFileColors(mode, selected, f, files)
        if showThisFile(f, mode, selected):
            if os.path.isdir(f):
                f = f + '/'
            width = max(width, len(f) + 1)
            writeText(t, x, y, f, fg, bg)
            #writeText(t, x, y, re.sub('grant', 'fouric', f), fg, bg)
            y += 1

def switchMode(prevMode, selected):
    "Switch the mode to either search or normal and do associated setup for each mode"
    if prevMode == Mode.SEARCH:
        newMode = Mode.NORMAL
        selected = files.index(selected[0]) if len(selected) else 0
    else:
        newMode = Mode.SEARCH
        selected = []
    searchBuffer = ''
    return (newMode, selected, searchBuffer)

def takeActionOnPath(f, path):
    "Do something with a filename that the user selected"
    if os.path.isdir(f):
        os.chdir(f)
        newMode = mode if persistentMode else defaultMode
        selected = 0 if newMode == Mode.NORMAL else []
        searchBuffer = ''
        return (newMode, selected, searchBuffer)
    elif os.path.isfile(f):
        mimetype = str(subprocess.check_output(['mimetype', f])).split(' ')[-1][:-3]
        for mapping in mimePatterns:
            if re.compile(mapping[0]).match(mimetype):
                escape = '\\' if 'escape-slash' in (mapping[3] if len(mapping) >= 4 else '') else ''
                runCommandOnFile(path, mapping[1] + ' ' + escape + '"' + f + escape + '"' + (mapping[2] if len(mapping) >= 3 else ''))

def runCommandOnFile(path, command):
    "Close lightning, write the current path, and execute the command"
    t.close()
    writePath(lightningPathFile, path)
    os.system(command)
    quit()

def slicer(num):
    "Returns a function that returns the first num characters of the given string"
    def f(s):
        return s[:num]
    return f

if __name__ == '__main__':
    try:
        mode = defaultMode
        searchBuffer = ''
        selected = selectedValueForMode(mode)
        files = None
        charRange = getCharRange()
        hiddenExpression = re.compile(hiddenFilesPattern)

        t = termbox.Termbox()
        while True:
            t.clear()
            if not files:
                files = sorted(os.listdir('.'))
                normalfiles = []
                dotfiles = []
                for f in files:
                    (dotfiles if hiddenExpression.match(f) else normalfiles).append(f)
                files = normalfiles
                if showHiddenFiles:
                    files += dotfiles
            if mode == Mode.SEARCH:
                selected = selectFilesOnsearchBuffer(files, searchBuffer)
            drawFileList(t, 1, t.height() - 1, t.width() - 1, mode, selected)
            if mode == Mode.SEARCH:
                if len(selected) == 1 and len(searchBuffer):
                    mode, selected, searchBuffer = takeActionOnPath(selected[0], os.path.realpath('.'))
                    files = None
                    continue
                writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
            modeText = 'search' if mode == Mode.SEARCH else 'normal'
            writeText(t, 0, 0, modeText + ": ", termbox.WHITE, 0)
            #writeText(t, len(modeText) + 2, 0, re.sub('grant', 'fouric', os.path.realpath('.')), termbox.WHITE, 0)
            writeText(t, len(modeText) + 2, 0, os.path.realpath('.'), termbox.WHITE, 0)
            t.present()

            event = t.poll_event()
            letter, keycode = event[1], event[2]
            if keycode == termbox.KEY_SPACE:
                mode, selected, searchBuffer = switchMode(mode, selected)
            elif letter == keybindings.KEY_UP_DIR:
                os.chdir('..')
                searchBuffer = ''
                if not persistentMode:
                    mode = defaultMode
                selected = selectedValueForMode(mode)
                files = None
            elif letter == keybindings.KEY_QUIT:
                runCommandOnFile(os.path.realpath('.'), 'true')
            elif letter == keybindings.KEY_SMART:
                mode, selected, searchBuffer = takeActionOnPath(files[files.index(selected[0]) if mode == Mode.SEARCH else selected], os.path.realpath('.'))
                files = None
            elif letter == keybindings.KEY_REFRESH:
                files = None
            elif letter == keybindings.KEY_TOGGLE_HIDDEN:
                showHiddenFiles = not showHiddenFiles
                files = None
            elif mode == Mode.NORMAL:
                if letter == keybindings.KEY_UP:
                    selected = (selected - 1) % len(files)
                elif letter == keybindings.KEY_DOWN:
                    selected = (selected + 1) % len(files)
                elif letter == keybindings.KEY_FILE_BROWSER:
                    runCommandOnFile(os.path.realpath('.'), fileBrowser + ' "' + os.path.realpath('.') + '" > /dev/null 2>&1')
                elif letter == keybindings.KEY_TMUX:
                    runCommandOnFile(os.path.realpath('.'), 'tmux > /dev/null')
            elif mode == Mode.SEARCH:
                if letter:
                    if letter in charRange:
                        if not restrictBuffer or (searchBuffer + letter) in list(map(slicer(len(searchBuffer) + 1), map(filenameClean, files))):
                            searchBuffer = searchBuffer + letter
                    elif letter == keybindings.KEY_DELETE:
                        searchBuffer = searchBuffer[:-1]

        t.close()
    except Exception as e:
        f = open(os.path.dirname(os.path.abspath(__file__)) + '/lightning-error.txt', 'w')
        f.write(traceback.format_exc() + '\n')
        f.close()
