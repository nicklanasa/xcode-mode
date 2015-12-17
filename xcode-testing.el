(require 'xcode-helpers)

(defun xcode-test-workspace ()
  "Test the Xcode workspace using xcodebuild."
  (interactive)
  (progn
    (let* ((workspace (xcode-select-workspace)))
      (xcode-compile
       (format "xcodebuild test -scheme %s -workspace %s -destination 'id=%s'"
               (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-workspace workspace) nil t)
               workspace
               (xcode-select-destination-id))))))

(provide 'xcode-testing)
