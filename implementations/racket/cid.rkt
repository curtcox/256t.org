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
(define (hex->bytes hex)
  (define cleaned (string-downcase (string-trim hex)))
  (define len (string-length cleaned))
  (unless (even? len)
    (error 'hex->bytes "expected even-length hex string"))
  (define out (make-bytes (/ len 2)))
  (for ([i (in-range 0 len 2)]
        [idx (in-naturals)])
    (define byte-val (string->number (substring cleaned i (+ i 2)) 16))
    (unless byte-val
      (error 'hex->bytes "invalid hex digits"))
    (bytes-set! out idx byte-val))
  out)

(define (sha512sum-digest sha512sum-path data-file)
  (define exit-code 0)
  (define output
    (with-output-to-string
      (lambda ()
        (set! exit-code (system*/exit-code sha512sum-path data-file)))))
  (define digest-hex (car (string-split (string-trim output))))
  (values (hex->bytes digest-hex) exit-code))

(define (sha512-bytes content)
  ;; Prefer openssl when available, otherwise fall back to sha512sum.
  (define openssl-path (find-executable-path "openssl"))
  (define sha512sum-path (find-executable-path "sha512sum"))
  (define data-file (make-temporary-file "cid-data-~a"))
  (define digest-file #f)
  (dynamic-wind
    void
    (lambda ()
      (call-with-output-file data-file #:exists 'truncate #:mode 'binary
        (lambda (out) (write-bytes content out)))
      (cond
        [openssl-path
         (set! digest-file (make-temporary-file "cid-digest-~a"))
         (define exit-code
           (system*/exit-code openssl-path "dgst" "-sha512" "-binary"
                              "-out" digest-file data-file))
         (cond
           [(zero? exit-code) (file->bytes digest-file)]
           [else
            (error 'sha512-bytes
                   (format "openssl sha512 failed with exit code ~a" exit-code))])]
        [sha512sum-path
         (define-values (digest exit-code)
           (sha512sum-digest sha512sum-path data-file))
         (cond
           [(zero? exit-code) digest]
           [else
            (error 'sha512-bytes
                   (format "sha512sum failed with exit code ~a" exit-code))])]
        [else
         (error 'sha512-bytes "no available sha512 executable (openssl or sha512sum)" )]))
    (lambda ()
      (when (file-exists? data-file) (delete-file data-file))
      (when (and digest-file (file-exists? digest-file)) (delete-file digest-file)))))

