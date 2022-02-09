(defun adjust-latex-scale (frame)
  (interactive)
  (if (and window-system (frame-size-changed-p frame))
      (setq org-format-latex-options
	    (cond ((> (frame-outer-height) 1080)
		   (plist-put org-format-latex-options :scale 3.0))
		  (t
		   (plist-put org-format-latex-options :scale 1.0))))))
(adjust-latex-scale nil)
(push 'adjust-latex-scale window-size-change-functions)