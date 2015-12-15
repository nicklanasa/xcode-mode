(require 'xcode-helpers)

(defun xcode-test-workspace()
  "Test the Xcode workspace using xcodebuild."
  (interactive)
  (progn
    (let* ((workspace 
            (completing-read
             "Select workspace: "
             (if (not (xcode-find-workspaces-for-directory default-directory))
                 (xcode-find-workspaces-for-directory ;; try one directory up
                  (file-name-directory (directory-file-name default-directory)))) nil t)))
      (xcode-compile
       (format "xcodebuild test -scheme %s -workspace %s -destination 'id=%s'"
               (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-workspace workspace) nil t)
               workspace
               (xcode-select-destination-id))))))

(provide 'xcode-testing)
