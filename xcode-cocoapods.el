(require 'xcode-helpers)

(defun xcode-run-pod-install()
  "Runs pod install."
  (interactive)
  (xcode-compile "pod install"))

(provide 'xcode-cocoapods)
