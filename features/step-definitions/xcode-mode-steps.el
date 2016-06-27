(And "^I wait for compilation to finish$"
     (lambda ()
       (setq ecukes--waiting-for-compilation t)

       (defun ecukes--compilation-finished (&rest ignore)
         (setq ecukes--waiting-for-compilation nil)
         (remove-hook 'compilation-finish-functions 'ecukes--compilation-finished))

       (add-hook 'compilation-finish-functions 'ecukes--compilation-finished)

       (while ecukes--waiting-for-compilation
         (accept-process-output nil 0.005))
       ))

(When "^I open a file \"\\([^\"]+\\)\"$"
      (lambda (arg)
	(find-file arg)))
