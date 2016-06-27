(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq xcode-mode-root-path project-directory))

(add-to-list 'load-path xcode-mode-root-path)

(require 'xcode-mode)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*testing-buffer*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (delete-selection-mode 0)
 (set-default 'indent-tabs-mode nil)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))
