(require 'subr-x)

(defun xcode-find-workspaces-for-directory(directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*workspace' -maxdepth 1" (substring directory 0 -1))))))

(defun xcode-find-projects-for-directory(directory)
  (mapcar #'string-trim
          (split-string
           (shell-command-to-string
            (format "find %s -name '*xcodeproj' -maxdepth 1" (substring directory 0 -1))))))

(defun xcode-find-schemes-for-workspace(workspace)
  (mapcar #'string-trim
          (cdr (cdr (split-string (shell-command-to-string
                                   (format "xcodebuild -workspace %s -list" workspace)) "\n")))))

(defvar xcode-platforms-list nil)

(defun xcode-get-platform-list()
  (setq xcode-platforms-list
        (cdr (assoc 'devices
                    (json-read-from-string
                     (shell-command-to-string "xcrun simctl list devices -j"))))))

(defun xcode-get-device-list(platform)
  (let ((platforms (cdr (assoc-if #'(lambda(str) (string= platform str))
                                  xcode-platforms-list))))
    (mapcar (lambda (element)
              (let ((device-list '()))
                (let ((device (cdr element))
                      (udid (cdr (car element))))
                  (cons (concat (cdr (assoc 'name device)) (concat " - " udid)) device-list))))
            (append platforms nil))))

(defun xcode-select-destination-id()
  (nth 3
       (split-string
        (completing-read "Select device:"
                         (xcode-get-device-list
                          (completing-read "Select platform:"
                                           (xcode-get-platform-list) nil t)) nil t))))

(defun xcode-select-sdk()
  (completing-read "Select SDK:" '("iphoneos" "macosx" "appletvos" "watchos") nil t))

(defun xcode-select-build-config()
  (completing-read "Select build config:" '("Debug" "Release") nil t))

(defun xcode-compile(command)
  (setq compilation-scroll-output t)
  (compile command))

(provide 'xcode-helpers)
