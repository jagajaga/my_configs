(defun indent-buffer ()
 "Indents an entire buffer using the default intenting scheme."
 (interactive)
 (point-to-register 'o)
 (delete-trailing-whitespace)
 (indent-region (point-min) (point-max) nil)
 (untabify (point-min) (point-max))
 (jump-to-register 'o))

