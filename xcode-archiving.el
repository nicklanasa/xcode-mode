(require 'xcode-helpers)

(defun xcode-archive-workspace()
  "Archive the Xcode project."
  (interactive)
  (progn
    (let* ((workspace (xcode-select-workspace)))
      (xcode-compile (format "xcodebuild archive -workspace %s -scheme %s -configuration Release"
														 workspace 
														 (completing-read
															"Select scheme: "
															(xcode-find-schemes-for-workspace workspace) nil t))))))

(provide 'xcode-archiving)


