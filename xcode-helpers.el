;; Copyright (C) 2015 Nickolas S Lanasa III

;; Author: Nickolas S Lanasa III <nick@nytekproductions.com>
;; Keywords: Xcode iOS

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(defcustom xcode-completing-read-function 'completing-read
  "Function to be called when requesting input from the user."
  :group 'xcode-mode
  :type '(radio (function-item completing-read)
                (function :tag "Other")))

(defmacro xcode-completing-read (&rest body)
  `(funcall xcode-completing-read-function ,@body))

(defun xcode-open-workspace ()
  "Open workspace in Xcode"
  (interactive)
	(shell-command
	 (format "open -a Xcode %s"
					 (xcode-completing-read
						"Select workspace: "
						(xcode-find-workspaces-for-directory default-directory) nil t))))

(defun xcode-open-project ()
  "Open project in Xcode"
  (interactive)
	(shell-command
	 (format "open -a Xcode %s"
					 (xcode-completing-read
						"Select project: "
						(xcode-find-projects-for-directory default-directory) nil t))))

(defun xcode-open (file)
  "Open file in Xcode"
  (interactive)
	(shell-command (format "open -a Xcode %s" file)))

(defun xcode-delete-derived-data ()
	(interactive)
	(xcode-compile
	 (format "rm -r ~/Library/Developer/Xcode/DerivedData/%s"
					 (xcode-completing-read
						"Select folder: "
						(xcode-derived-data-list) nil t))))

(defun xcode-derived-data-list ()
	(mapcar #'string-trim
					(split-string
					 (shell-command-to-string "ls ~/Library/Developer/Xcode/DerivedData/"))))

(defun xcode-find-binaries ()
	(mapcar #'string-trim
					(split-string
					 (shell-command-to-string "find ~/Library/Developer/Xcode/DerivedData/ -name '*.app'"))))

(defun xcode-find-storyboards-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*storyboard'" (substring directory 0 -1))))))

(defun xcode-select-project ()
	(xcode-completing-read
	 "Select project: "
	 (xcode-find-projects-for-directory default-directory) nil t))

(defun xcode-select-workspace ()
	(xcode-completing-read
	 "Select workspace: "
	 (xcode-find-workspaces-for-directory default-directory) nil t))

(defun xcode-find-workspaces-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*workspace'" directory)))))

(defun xcode-find-projects-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*xcodeproj'")))))

(defun xcode-find-schemes-for-workspace (workspace)
  (mapcar #'string-trim
          (cdr (cdr (split-string (shell-command-to-string
                                   (format "xcodebuild -workspace %s -list" workspace)) "\n")))))

(defun xcode-find-schemes-for-project (project)
  (mapcar #'string-trim
          (cdr (cdr (split-string (shell-command-to-string
                                   (format "xcodebuild -project %s -list" project)) "\n")))))


(defvar xcode-platforms-list nil)

(defun xcode-get-platform-list ()
  (setq xcode-platforms-list
        (cdr (assoc 'devices
                    (json-read-from-string
                     (shell-command-to-string "xcrun simctl list devices -j"))))))

(defun xcode-get-device-list (platform)
  (let ((platforms (cdr (cl-assoc-if #'(lambda(str) (string= platform str))
																		 xcode-platforms-list))))
    (mapcar (lambda (element)
              (let ((device-list '()))
                (let ((device (cdr element))
                      (udid (cdr (car element))))
                  (cons (concat (cdr (assoc 'name device)) (concat " - " udid)) device-list))))
            (append platforms nil))))

(defun xcode-select-destination-id ()
  (nth 3
       (split-string
        (xcode-completing-read "Select device:"
                         (xcode-get-device-list
                          (xcode-completing-read "Select platform:"
                                           (xcode-get-platform-list) nil t)) nil t))))

(defun xcode-select-sdk ()
  (xcode-completing-read "Select SDK:" '("iphoneos" "macosx" "appletvos" "watchos" "iphonesimulator") nil t))

(defun xcode-select-build-config ()
  (xcode-completing-read "Select build config:" '("Debug" "Release") nil t))

(defun xcode-compile (command)
  (xcode-in-root
   (compile command)))

(defun xcode-use-xctool ()
	(setq xcode-xctool-path "/usr/local/bin/xctool"))

(defmacro xcode-in-root (body)
  "Execute BODY form with `xcode-project-directory' as
``default-directory''."
  `(let ((default-directory (xcode-project-directory)))
     ,body))

(defun xcode-project-directory ()
  "Get project directory.
If projectile available, use projectile.
Else, use locate-dominating-file.
Finally fall back to the current directory."
  (if (not (fboundp 'projectile-project-root))
      (projectile-project-root)
    (or (locate-dominating-file default-directory ".xctool-args")
        default-directory)))

(provide 'xcode-helpers)
