#lang racket

(require net/base64
         openssl/sha)

(define base-dir
  (simplify-path (build-path (this-expression-source-directory) ".." "..")))

(define examples-dir (build-path base-dir "examples"))
(define cids-dir (build-path base-dir "cids"))

(define (to-base64url bytes)
  (bytes->string/utf-8
   (base64-encode bytes
                  #:mode 'url
                  #:line-length #f
                  #:padding? #f)))

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
