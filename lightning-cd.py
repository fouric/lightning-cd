#!/usr/bin/python3

import traceback
import termbox
import os
import sys
import re
import subprocess

sys.path.append(os.path.dirname(os.path.abspath(__file__)))
import settings
import keybindings

def writeText(t, x, y, text, foreground, background):
    """writeText(t Termbox, x integer, x integer, fg termbox.COLOR, fg termbox.COLOR) -> None
    Execute a series of change_cell's in a sequential manner such as to write a line of text"""
    for i in range(len(text)):
        t.change_cell(x + i, y, ord(text[i]), foreground, background)

def getCharRange():
    """getCharRange() -> str
    Get a string of the characters that are valid to enter in search mode"""
    chars = ''
    for i in range(255):
        if (i >= ord('a') and i <= ord('z')) or (i >= ord('0') and i <= ord('9')):
            chars = chars + chr(i)
    chars = chars + '.'
    return chars

def filenameClean(filename):
    """filenameClean(filename str) -> str
    Convert a raw filename to a simplified one that can be searched for"""
    newFilename = ''
    filename = filename.lower()
    acceptableChars = getCharRange()
    for char in filename:
        if char in acceptableChars:
            newFilename = newFilename + char
    return newFilename

def selectFilesOnsearchBuffer(files, searchBuffer):
    """selectFilesOnsearchBuffer(files list[str], searchBuffer str) -> list[str]
    Return a list of selected files by comparing simplified filenames with the search buffer"""
    selected = []
    for f in files:
        if searchBuffer == filenameClean(f)[:len(searchBuffer)]:
            selected.append(f)
    return selected

def getFileColors(mode, selected, thisFile, fileList):
    """getFileColors(mode Enum, selected int, thisFile string, fileList list[str]) -> (termbox.COLOR, termbox.COLOR)
    Takes a bunch of things and returns the color that the given file should be when displayed"""
    if mode == settings.Mode.SEARCH and thisFile in selected and settings.showDeselectedFiles:
        fg, bg = termbox.BLACK, termbox.WHITE
    elif mode == settings.Mode.NORMAL and thisFile == fileList[selected]:
        fg, bg = termbox.BLACK, termbox.WHITE
    else:
        fg, bg = termbox.BLUE if os.path.isdir(thisFile) else termbox.WHITE, 0
    return (fg, bg)

def showThisFile(thisFile, mode, selected):
    """showThisFile(string, Enum, integer) -> bool
    Returns whether or not the given file should be actually displayed"""
    return settings.showDeselectedFiles or mode == settings.Mode.NORMAL or thisFile in selected or selected == []

def writePath(filename, path):
    """writePath(filename str, path str) -> None
    Write a string (usually the path that the calling shell should cd to) to a file"""
    f = open(filename, 'w+')
    f.write(path)
    f.close()

def selectedValueForMode(mode):
    """selectedValueForMode(mode Enum) -> list/int
    Return the correct default value for 'selected', which can either be a list or an int"""
    return [] if mode == settings.Mode.SEARCH else 0

def drawFileList(t, ystart, yend, xend, mode, selected):
    """drawFileList(t Termbox, ystart integer, yend integer, xend integer, mode Enum, selected int/list) -> None
    Draw the list of selected files onto the screen"""
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
    """switchMode(prevMode Enum, selected int/list) -> (Enum, int/list, str)
    Switch the mode to either search or normal and do associated setup for each mode"""
    if prevMode == settings.Mode.SEARCH:
        newMode = settings.Mode.NORMAL
        selected = files.index(selected[0]) if len(selected) else 0
    else:
        newMode = settings.Mode.SEARCH
        selected = []
    searchBuffer = ''
    return (newMode, selected, searchBuffer)

def takeActionOnPath(f, path):
    """takeActionOnPath(f str, path str) -> (Enum, int/list, str)
    Do something with a filename that the user selected"""
    if os.path.isdir(f):
        os.chdir(f)
        newMode = mode if settings.persistentMode else settings.defaultMode
        selected = 0 if newMode == settings.Mode.NORMAL else []
        searchBuffer = ''
        return (newMode, selected, searchBuffer)
    elif os.path.isfile(f):
        mimetype = str(subprocess.check_output(['mimetype', f])).split(' ')[-1][:-3]
        for mapping in settings.mimePatterns:
            if re.compile(mapping[0]).match(mimetype):
                escape = '\\' if 'escape-slash' in (mapping[3] if len(mapping) >= 4 else '') else ''
                runCommandOnFile(path, mapping[1] + ' ' + escape + '"' + f + escape + '"' + (mapping[2] if len(mapping) >= 3 else ''))

def runCommandOnFile(path, command):
    """runCommandOnFile(path str, command str) -> [doesn't return]
    Close lightning, write the current path, and execute the command"""
    t.close()
    writePath(settings.lightningPathFile, path)
    os.system(command)
    quit()

def slicer(num):
    """slicer(num int) -> f(int)
    Returns a function that returns the first num characters of the given string"""
    def f(s):
        return s[:num]
    return f

if __name__ == '__main__':
    try:
        mode = settings.defaultMode
        searchBuffer = ''
        selected = selectedValueForMode(mode)
        files = None
        charRange = getCharRange()
        hiddenExpression = re.compile(settings.hiddenFilesPattern)

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
                if settings.showHiddenFiles:
                    files += dotfiles
            if mode == settings.Mode.SEARCH:
                selected = selectFilesOnsearchBuffer(files, searchBuffer)
            drawFileList(t, 1, t.height() - 1, t.width() - 1, mode, selected)
            if mode == settings.Mode.SEARCH:
                if len(selected) == 1 and len(searchBuffer):
                    mode, selected, searchBuffer = takeActionOnPath(selected[0], os.path.realpath('.'))
                    files = None
                    continue
                writeText(t, 0, t.height() - 1, searchBuffer, termbox.WHITE, 0)
            modeText = 'search' if mode == settings.Mode.SEARCH else 'normal'
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
                if not settings.persistentMode:
                    mode = settings.defaultMode
                selected = selectedValueForMode(mode)
                files = None
            elif letter == keybindings.KEY_QUIT:
                runCommandOnFile(os.path.realpath('.'), 'true')
            elif letter == keybindings.KEY_SMART:
                mode, selected, searchBuffer = takeActionOnPath(files[files.index(selected[0]) if mode == settings.Mode.SEARCH else selected], os.path.realpath('.'))
                files = None
            elif letter == keybindings.KEY_REFRESH:
                files = None
            elif letter == keybindings.KEY_TOGGLE_HIDDEN:
                settings.showHiddenFiles = not settings.showHiddenFiles
                files = None
            elif mode == settings.Mode.NORMAL:
                if letter == keybindings.KEY_UP:
                    selected = (selected - 1) % len(files)
                elif letter == keybindings.KEY_DOWN:
                    selected = (selected + 1) % len(files)
                elif letter == keybindings.KEY_FILE_BROWSER:
                    runCommandOnFile(os.path.realpath('.'), fileBrowser + ' "' + os.path.realpath('.') + '" > /dev/null 2>&1')
                elif letter == keybindings.KEY_TMUX:
                    runCommandOnFile(os.path.realpath('.'), 'tmux > /dev/null')
            elif mode == settings.Mode.SEARCH:
                if letter:
                    if letter in charRange:
                        if not settings.restrictBuffer or (searchBuffer + letter) in list(map(slicer(len(searchBuffer) + 1), map(filenameClean, files))):
                            searchBuffer = searchBuffer + letter
                    elif letter == keybindings.KEY_DELETE:
                        searchBuffer = searchBuffer[:-1]

        t.close()
    except Exception as e:
        f = open(os.path.dirname(os.path.abspath(__file__)) + '/lightning-error.txt', 'w')
        f.write(traceback.format_exc() + '\n')
        f.close()
