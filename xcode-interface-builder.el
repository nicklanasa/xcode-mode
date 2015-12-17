(defun xcode-open-storyboard ()
  "Select and open storyboard."
  (interactive)
	(xcode-open (completing-read
							 "Select storyboard: "
							 (xcode-find-storyboards-for-directory default-directory) nil t)))

(provide 'xcode-interface-builder)
