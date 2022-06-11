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

;; TODO: Handle collapsd text like embedded links (call buffer substring after removing tags?)
(defun find-longest-region-line (beg end)
  "Find the longest line in region."
  (message "Region size beginning %d, end %d" beg end)
  (let ((width-max 0))
    (while (not (eobp))
      (let* ((line-start (save-excursion (forward-line 0) (point)))
             (line-end (save-excursion (forward-line 1) (- (point) 1)))
             (line (buffer-substring-of-visible line-start line-end)))
        (if (char-equal (char-after line-start) ?* )
            (let ((real-len (org-calc-length-minus-tags line)))
              (when (> real-len width-max) (setq width-max real-len)))))
      (forward-line 1))
    (message "Setting tags column to %d" width-max)
    (eval width-max)))

(defun org-calc-length-minus-tags (line)
  "Identifies a strings actual length ignoring any org tags"
  (let* ((trimmed-str (replace-regexp-in-string " *\\(:[^:]+\\)+:$" "" line))
         (trimmed-len (string-width trimmed-str)))
    (eval (+ trimmed-len 2))))

(provide 'org-tagging-support)

;;; org-tagging-support.el ends here
