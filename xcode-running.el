;; Copyright (C) 2016 Nickolas S Lanasa III

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

(defun xcode-xctool-run ()
	"Runs the Xcode project in sim using xctool."
  (interactive)
	(compile
	 (format "ios-sim launch %s --devicetypeid '%s'"
					 (xcode-completing-read
						"Select app: "
						(xcode-find-binaries) nil t)
					 xcode-ios-sim-devicetype)))

(defun xcode-xctool-build-and-run ()
	"Build and run the in sim using xctool"
	(interactive)
	(message "Building...")
	(shell-command-to-string "xctool build")
	(compile
	 (format "ios-sim launch %s --devicetypeid '%s'"
					 (xcode-completing-read
						"Select app: "
						(xcode-find-binaries) nil t)
					 xcode-ios-sim-devicetype)))

(provide 'xcode-running)
