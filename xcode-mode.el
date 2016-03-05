;;; xcode-mode.el --- A minor mode for emacs to perform Xcode like actions.

;; Copyright (C) 2015 Nickolas Lanasa

;; Author: Nickolas Lanasa <nick@nytekproductions.com>,
;; Keywords: conveniences
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (s "1.10.0") (dash "2.11.0") (multiple-cursors "1.0.0"))

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

(defvar xcode-mode-map
  (make-sparse-keymap)
  "Keymap for xcode.")

;; com.apple.CoreSimulator.SimDeviceType.iPhone-4s, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-5, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-5s, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-6, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-6-Plus, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-6s, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPhone-6s-Plus, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPad-2, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPad-Retina, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPad-Air, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPad-Air-2, 9.2
;; com.apple.CoreSimulator.SimDeviceType.iPad-Pro, 9.2
;; com.apple.CoreSimulator.SimDeviceType.Apple-TV-1080p, 9.1
;; com.apple.CoreSimulator.SimDeviceType.Apple-Watch-38mm, 2.1
;; com.apple.CoreSimulator.SimDeviceType.Apple-Watch-42mm, 2.1

(defvar xcode-ios-sim-devicetype "com.apple.CoreSimulator.SimDeviceType.iPhone-6, 9.2")

(require 'xcode-helpers)
(require 'xcode-building)
(require 'xcode-cleaning)
(require 'xcode-cocoapods)
(require 'xcode-testing)
(require 'xcode-archiving)
(require 'xcode-running)
(require 'xcode-interface-builder)

(setq compilation-scroll-output t)
  
;;;###autoload
(define-minor-mode xcode-mode
  "Minor mode to perform xcode like actions."
  :lighter " xcode"
  :keymap xcode-mode-map)

;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Build then run
(define-key xcode-mode-map
  (kbd"C-c C-x br") 'xcode-xctool-build-and-run)

(define-key xcode-mode-map
  (kbd"C-c C-x rr") 'xcode-xctool-run)

;; Building
(define-key xcode-mode-map
  (kbd"C-c C-x bb") 'xcode-xctool-build)

;; Testing
(define-key xcode-mode-map
  (kbd "C-c C-x rt") 'xcode-xctool-run-tests)
(define-key xcode-mode-map
  (kbd "C-c C-x bt") 'xcode-xctool-build-tests)
(define-key xcode-mode-map
  (kbd "C-c C-x tt") 'xcode-xctool-test)

;; Cleaning
(define-key xcode-mode-map
  (kbd "C-c C-x cc") 'xcode-xctool-clean)

;; Cocoapods
(define-key xcode-mode-map
  (kbd "C-c C-x pi") 'xcode-pod-install)

;; Opening in Xcode
(define-key xcode-mode-map
  (kbd "C-c C-x os") 'xcode-open-storyboard)
(define-key xcode-mode-map
  (kbd "C-c C-x op") 'xcode-open-project)
(define-key xcode-mode-map
  (kbd "C-c C-x ow") 'xcode-open-workspace)

;; Archiving
(define-key xcode-mode-map
  (kbd "C-c C-x aa") 'xcode-xctool-archive)

;; Helpers
(define-key xcode-mode-map
  (kbd "C-c C-x dd") 'xcode-delete-derived-data)

(provide 'xcode-mode)
;;; xcode-mode.el ends here
