(require 'xcode-helpers)

(defun xcode-archive-workspace ()
  "Archive the Xcode workspace."
  (interactive)
  (progn
    (let* ((workspace (xcode-select-workspace)))
      (xcode-compile (format "xcodebuild archive -workspace %s -scheme %s -configuration Release"
														 workspace 
														 (completing-read
															"Select scheme: "
															(xcode-find-schemes-for-workspace workspace) nil t))))))

(defun xcode-archive-project ()
  "Archive the Xcode project."
  (interactive)
  (progn
    (let* ((project (xcode-select-project)))
      (xcode-compile (format "xcodebuild archive -project %s -scheme %s -configuration Release"
														 project 
														 (completing-read
															"Select scheme: "
															(xcode-find-schemes-for-project project) nil t))))))


(provide 'xcode-archiving)


