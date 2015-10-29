import os
from enum import Enum

showDeselectedFiles = False
Mode = Enum('Mode', 'NORMAL SEARCH')
defaultMode = Mode.SEARCH
editor = 'nvim'
fileBrowser = 'nautilus'
persistentMode = True
lightningPathFile = os.sys.argv[1]
showHiddenFiles = False
hiddenFilesPattern = '(^\\..*)|(.*~$)|(.*\\.(swp|pyc|fasl|o|spec|bak)$)|(__pycache__?.)'
restrictBuffer = True
