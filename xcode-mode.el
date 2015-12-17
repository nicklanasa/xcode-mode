;;; xcode-mode.el --- A minor mode for emacs to perform Xcode like actions.

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

;;; Commentary:

;; This is a collection of functions to further the idea of a
;; using Emacs to perform Xcode like actions.

(require 'xcode-building)
(require 'xcode-cleaning)
(require 'xcode-cocoapods)
(require 'xcode-testing)
(require 'xcode-archiving)
(require 'xcode-interface-builder)

(defvar xcode-mode-map
  (make-sparse-keymap)
  "Keymap for xcode.")
  
;;;###autoload
(define-minor-mode xcode-mode
  "Minor mode to perform xcode like actions."
  :lighter " xcode"
  :keymap xcode-mode-map)

;;; Keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key xcode-mode-map
  (kbd "C-c C-x bw") 'xcode-build-workspace)
(define-key xcode-mode-map
  (kbd "C-c C-x bp") 'xcode-build-project)
(define-key xcode-mode-map
  (kbd "C-c C-x tw") 'xcode-test-workspace)
(define-key xcode-mode-map
  (kbd "C-c C-x cw") 'xcode-clean-workspace)
(define-key xcode-mode-map
  (kbd "C-c C-x pi") 'xcode-pod-install)
(define-key xcode-mode-map
  (kbd "C-c C-x os") 'xcode-open-storyboard)
(define-key xcode-mode-map
  (kbd "C-c C-x aw") 'xcode-archive-workspace)
(define-key xcode-mode-map
  (kbd "C-c C-x ap") 'xcode-archive-project)
(define-key xcode-mode-map
  (kbd "C-c C-x op") 'xcode-open-project)
(define-key xcode-mode-map
  (kbd "C-c C-x ow") 'xcode-open-workspace)


(provide 'xcode-mode)
;;; xcode-mode.el ends here
