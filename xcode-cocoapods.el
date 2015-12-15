(require 'xcode-helpers)

(defun xcode-pod-install()
  "Runs pod install."
  (interactive)
  (xcode-compile "pod install"))

(provide 'xcode-cocoapods)
