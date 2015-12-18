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

(defun xcode-open-workspace ()
  "Open workspace in Xcode"
  (interactive)
	(shell-command
	 (format "open -a Xcode %s"
					 (completing-read
						"Select workspace: "
						(if (not (xcode-find-workspaces-for-directory default-directory))
								(xcode-find-workspaces-for-directory ;; try one directory up
								 (file-name-directory (directory-file-name default-directory)))) nil t))))

(defun xcode-open-project ()
  "Open project in Xcode"
  (interactive)
	(shell-command
	 (format "open -a Xcode %s"
					 (completing-read
						"Select project: "
						(if (not (xcode-find-projects-for-directory default-directory))
								(xcode-find-projects-for-directory ;; try one directory up
								 (file-name-directory (directory-file-name default-directory)))) nil t))))

(defun xcode-open (file)
  "Open file in Xcode"
  (interactive)
	(shell-command (format "open -a Xcode %s" file)))

(defun xcode-find-storyboards-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*storyboard' -maxdepth 1" (substring directory 0 -1))))))

(defun xcode-select-project ()
	(completing-read
	 "Select project: "
	 (if (not (xcode-find-projects-for-directory default-directory))
			 (xcode-find-projects-for-directory ;; try one directory up
				(file-name-directory (directory-file-name default-directory)))) nil t))

(defun xcode-select-workspace ()
		(completing-read
		 "Select workspace: "
		 (if (not (xcode-find-workspaces-for-directory default-directory))
				 (xcode-find-workspaces-for-directory ;; try one directory up
					(file-name-directory (directory-file-name default-directory)))) nil t))

(defun xcode-find-workspaces-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*workspace' -maxdepth 1" (substring directory 0 -1))))))

(defun xcode-find-projects-for-directory (directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*xcodeproj' -maxdepth 1" (substring directory 0 -1))))))

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
        (completing-read "Select device:"
                         (xcode-get-device-list
                          (completing-read "Select platform:"
                                           (xcode-get-platform-list) nil t)) nil t))))

(defun xcode-select-sdk ()
  (completing-read "Select SDK:" '("iphoneos" "macosx" "appletvos" "watchos") nil t))

(defun xcode-select-build-config ()
  (completing-read "Select build config:" '("Debug" "Release") nil t))

(defun xcode-compile (command)
  (setq compilation-scroll-output t)
  (compile command))

(provide 'xcode-helpers)
