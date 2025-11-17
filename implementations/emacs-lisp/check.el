;;; check.el --- Verify CID files  -*- lexical-binding: t; -*-

(load-file (expand-file-name "cid.el" (file-name-directory (or load-file-name buffer-file-name))))

(defun cid-check-all ()
  (let ((mismatches '())
        (count 0))
    (dolist (path (sort (directory-files cid-cids-dir t "^[^.]") #'string<))
      (when (file-regular-p path)
        (setq count (1+ count))
        (let* ((actual (file-name-nondirectory path))
               (expected (cid-compute (cid-read-file path))))
          (unless (string= actual expected)
            (push (cons actual expected) mismatches)))))
    (if mismatches
        (progn
          (princ "Found CID mismatches:\n")
          (dolist (pair (reverse mismatches))
            (princ (format "- %s should be %s\n" (car pair) (cdr pair))))
          1)
      (princ (format "All %d CID files match their contents.\n" count))
      0)))

(let ((status (cid-check-all)))
  (kill-emacs status))

;;; check.el ends here
