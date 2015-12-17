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

(require 'xcode-helpers)

(defun xcode-clean-workspace()
  "Cleans the Xcode workspace using xcodebuild."
  (interactive)
  (progn
    (let* ((workspace 
            (completing-read
             "Select workspace: "
             (if (not (xcode-find-workspaces-for-directory default-directory))
                 (xcode-find-workspaces-for-directory ;; try one directory up
                  (file-name-directory (directory-file-name default-directory)))) nil t)))
      (xcode-compile
       (format "xcodebuild clean -scheme %s -workspace %s"
               (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-workspace workspace) nil t)
               workspace)))))

(provide 'xcode-cleaning)
