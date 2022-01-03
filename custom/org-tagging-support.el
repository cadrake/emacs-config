;;; $DOOMDIR/custom/org-tagging-support.el -*- lexical-binding: t; -*-

(require 'org)
(require 'subr+)

;;;###autoload
(defun org-set-tag-column-and-align ()
  "In an org mode buffer, find the longest line in the displayed region and set `org-tags-column' to that column"
  (interactive)
  (when (and
         (not (string= (buffer-name) "*Remember*"))
         (eql major-mode 'org-mode))
    (setq org-tags-column (find-longest-region-line (window-start) (window-end)))
    (org-align-tags t)))

(defun find-longest-region-line (beg end)
  "Find the longest line in region."
  (message (format "Region size beginning %d, end %d" beg end))
  (let* ((real-end     (save-excursion (goto-char end) (end-of-line) (point)))
         (buf-str      (buffer-substring-of-visible beg real-end))
         (lines        (split-string buf-str "\n"))
         (width-max    0)
         (count        0))
    (cl-loop for i in lines
             do (progn
                  (let ((real-len (org-calc-length-minus-tags i)))
                      (when (> real-len width-max)
                        (setq width-max real-len))
                    (cl-incf count))))
    (message (format "Setting tags column to %d" width-max))
    (eval width-max)))

(defun org-calc-length-minus-tags (line)
  "Identifies a strings actual length ignoring any org tags"
  (let* ((trimmed-str (replace-regexp-in-string " *\\(:[^:]+\\)+:$" "" line))
         (trimmed-len (string-width trimmed-str)))
  (eval (+ trimmed-len 2))))

(provide 'org-tagging-support)

;;; org-tagging-support.el ends here
