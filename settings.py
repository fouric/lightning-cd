import os
from enum import Enum

Mode = Enum('Mode', 'NORMAL SEARCH')
defaultMode = Mode.SEARCH
fileBrowser = 'nautilus'
persistentMode = True
lightningPathFile = os.sys.argv[1]
showHiddenFiles = False
hiddenFilesPattern = '(^\\..*)|(.*~$)|(.*\\.(swp|pyc|fasl|o|spec|bak)$)|(__pycache__?.)'
restrictBuffer = True
mimePatterns = [('text\\/.*', 'nvim'), ('application\\/x-shellscript', 'nvim'), ('application\\/x-executable', 'bash -c "exec', '"', 'escape-slash'), ('.*', 'xdg-open')]
