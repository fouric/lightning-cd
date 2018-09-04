#!/usr/bin/python3

import termbox

t = termbox.Termbox()

t.clear()

width = t.width()
height = t.height()
cell_count = width * height
char = ord('a')
for c in range(1):
    for i in range(26):
        for y in range(height):
            for x in range(width):
                t.change_cell(x, y, char, termbox.WHITE, termbox.BLACK)
                t.present()
        char += 1

t.close()
