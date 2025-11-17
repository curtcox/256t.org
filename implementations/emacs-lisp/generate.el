;;; generate.el --- Generate CID files from examples  -*- lexical-binding: t; -*-

(load-file (expand-file-name "cid.el" (file-name-directory (or load-file-name buffer-file-name))))

(defun cid-generate ()
  (make-directory cid-cids-dir t)
  (dolist (path (sort (directory-files cid-examples-dir t "^[^.]") #'string<))
    (when (file-regular-p path)
      (let* ((content (cid-read-file path))
             (cid (cid-compute content))
             (destination (expand-file-name cid cid-cids-dir)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert content)
          (write-region (point-min) (point-max) destination nil 'silent))
        (princ (format "Wrote %s from %s\n" (file-name-nondirectory destination)
                       (file-name-nondirectory path))))))
  0)

(let ((status (cid-generate)))
  (kill-emacs status))

;;; generate.el ends here
