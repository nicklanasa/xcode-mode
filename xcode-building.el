(require 'xcode-helpers)

(defun xcode-build-project ()
  "Builds the Xcode project using xcodebuild."
  (interactive)
  (xcode-compile (format "xcodebuild -project %s -sdk %s -configuration %s"
												 (xcode-select-project)
												 (xcode-select-sdk)
												 (xcode-select-build-config))))

(defun xcode-build-workspace ()
  "Builds the Xcode workspace using xcodebuild."
  (interactive)
  (progn
    (let* ((workspace (xcode-select-workspace)))
      (xcode-compile (format "xcodebuild build -scheme %s -workspace %s -sdk %s -configuration %s"
														 (completing-read
															"Select scheme: "
															(xcode-find-schemes-for-workspace workspace) nil t)
														 workspace
														 (xcode-select-sdk)
														 (xcode-select-build-config))))))

(provide 'xcode-building)
