(require 'xcode-helpers)

(defun xcode-clean-workspace()
  "Cleans the Xcode workspace using xcodebuild."
  (interactive)
  (progn
    (let* ((workspace 
            (completing-read
             "Select workspace: "
             (if (not (xcode-find-workspaces-for-directory default-directory))
                 (xcode-find-workspaces-for-directory ;; try one directory up
                  (file-name-directory (directory-file-name default-directory)))) nil t)))
      (xcode-compile
       (format "xcodebuild clean -scheme %s -workspace %s"
               (completing-read
                "Select scheme: "
                (xcode-find-schemes-for-workspace workspace) nil t)
               workspace)))))

(provide 'xcode-cleaning)
