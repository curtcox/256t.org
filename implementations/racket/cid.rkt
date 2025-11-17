#lang racket

(require racket/runtime-path
         racket/string
         racket/system
         net/base64)

(define-runtime-path here ".")

(define base-dir
  (simplify-path (build-path here ".." "..")))

(define examples-dir (build-path base-dir "examples"))
(define cids-dir (build-path base-dir "cids"))

(define (to-base64url bytes)
  (define base64-str
    (bytes->string/utf-8 (base64-encode bytes)))
  (define cleaned
    (regexp-replace* #px"[\r\n]+" base64-str ""))
  (define urlized
    (string-replace (string-replace cleaned "+" "-") "/" "_"))
  (regexp-replace #px"=+$" urlized ""))

(define (encode-length length)
  (define (byte-at shift)
    (bitwise-and (arithmetic-shift length (- shift)) #xFF))
  (to-base64url
   (bytes (byte-at 40)
          (byte-at 32)
          (byte-at 24)
          (byte-at 16)
          (byte-at 8)
          (byte-at 0))))

(define (compute-cid content)
  (define prefix (encode-length (bytes-length content)))
  (define suffix
    (if (<= (bytes-length content) 64)
        (to-base64url content)
        (to-base64url (sha512-bytes content))))
  (string-append prefix suffix))

(provide base-dir
         examples-dir
         cids-dir
         compute-cid)
(define (sha512-bytes content)
  ;; Use temporary files to avoid dependence on specific subprocess port orderings
  (define data-file (make-temporary-file "cid-data-~a"))
  (define digest-file (make-temporary-file "cid-digest-~a"))
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file data-file #:exists 'truncate #:mode 'binary
        (lambda (out) (write-bytes content out)))
      (define exit-code
        (system*/exit-code "openssl" "dgst" "-sha512" "-binary"
                           "-out" digest-file data-file))
      (cond
        [(zero? exit-code) (file->bytes digest-file)]
        [else
         (error 'sha512-bytes
                (format "openssl sha512 failed with exit code ~a" exit-code))]))
    (lambda ()
      (when (file-exists? data-file) (delete-file data-file))
      (when (file-exists? digest-file) (delete-file digest-file)))))

