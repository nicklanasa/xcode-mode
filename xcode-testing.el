;; Copyright (C) 2015 Nickolas Lanasa

;; Author: Nickolas Lanasa <nick@nytekproductions.com>,
;; Keywords: conveniences

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

(defun xcode-xctool-run-tests ()
	"Tests a project using .xctool-args file in current directory."
	(interactive)
	(xcode-compile "xctool run-tests"))

(defun xcode-xctool-build-tests ()
	"Tests a project using .xctool-args file in current directory."
	(interactive)
	(xcode-compile "xctool build-tests"))

(defun xcode-test-workspace ()
  "Test the Xcode workspace using xctool."
  (interactive)
  (progn
    (let* ((workspace (xcode-select-workspace)))
      (xcode-compile
       (format "%s -scheme %s -workspace %s -destination 'id=%s' -sdk %s test"
							 xcode-xctool-path
               (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-workspace workspace) nil t)
               workspace
               (xcode-select-destination-id)
							 (xcode-select-sdk))))))

(defun xcode-test-project ()
  "Test the Xcode project using xctool."
  (interactive)
  (progn
    (let* ((project (xcode-select-project)))
      (xcode-compile
       (format "%s test -scheme %s -project %s -destination 'id=%s'"
							 xcode-xctool-path
							 (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-project project) nil t)
               project
               (xcode-select-destination-id))))))


(provide 'xcode-testing)
