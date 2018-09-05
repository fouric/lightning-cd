(in-package :lightning-cd)

(proclaim '(optimize (speed 0) (safety 0) (space 0) (debug 3)))

(defparameter *default-mode* :search)
(defparameter *editor* "nvim")
(defparameter *lightning-initial-path-file* "/home/fouric/ramdisk/.lightninginitialpath")
(defparameter *lightning-path-file* "/home/fouric/ramdisk/.lightningpath")
(defparameter *lightning-command-file* "/home/fouric/ramdisk/.lightningcommand")
(defparameter *current-directory* nil)
