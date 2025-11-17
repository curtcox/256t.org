;;; cid.el --- Compute CIDs  -*- lexical-binding: t; -*-

(defconst cid--base-dir
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root directory.")

(defconst cid-examples-dir (expand-file-name "examples" cid--base-dir))
(defconst cid-cids-dir (expand-file-name "cids" cid--base-dir))

(defun cid--trim-padding (encoded)
  "Remove trailing padding characters from ENCODED base64 string."
  (let ((i (length encoded)))
    (while (and (> i 0) (eq (aref encoded (1- i)) ?=))
      (setq i (1- i)))
    (substring encoded 0 i)))

(defun cid--base64url (data)
  "Return base64url encoding of DATA.
DATA should be a unibyte string."
  (let ((encoded (base64-encode-string data t)))
    (setq encoded (cid--trim-padding encoded))
    (setq encoded (subst-char-in-string ?+ ?- encoded t))
    (subst-char-in-string ?/ ?_ encoded t)))

(defun cid--encode-length (length)
  "Encode LENGTH as a 6-byte big-endian base64url string."
  (let ((bytes (make-string 6 0)))
    (dotimes (i 6)
      (aset bytes i (logand (lsh length (* -8 (- 5 i))) #xff)))
    (cid--base64url bytes)))

(defun cid-compute (content)
  "Compute the CID for CONTENT bytes (unibyte string)."
  (let* ((len (length content))
         (prefix (cid--encode-length len))
         (suffix (if (<= len 64)
                     (cid--base64url content)
                   (cid--base64url (secure-hash 'sha512 content nil nil t)))))
    (concat prefix suffix)))

(defun cid-read-file (path)
  "Read file at PATH as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(provide 'cid)

;;; cid.el ends here
