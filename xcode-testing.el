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

(defun xcode-xctool-test ()
  "Test the Xcode project using xctool."
  (interactive)
	(xcode-compile "xctool test"))

(defun xcode-xctool-test-only (scheme)
  "Test the Xcode project using xctool with a specific scheme."
	(interactive "sEnter scheme: ")
	(xcode-compile (format "xctool test -only %s" scheme)))

(defun xcode-xctool-run-tests-only (scheme)
  "Runs tests the Xcode project using xctool with a specific scheme."
	(interactive "sEnter scheme: ")
	(xcode-compile (format "xctool run-tests -only %s" scheme)))

(defun xcode-xctool-build-tests-only (scheme)
  "Builds tests the Xcode project using xctool with a specific scheme."
	(interactive "sEnter scheme: ")
	(xcode-compile (format "xctool build-tests -only %s" scheme)))


(provide 'xcode-testing)
