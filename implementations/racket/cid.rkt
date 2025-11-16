#lang racket

(require net/base64)

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
(define (sha512-bytes content)
  (define in-port (open-input-bytes content))
  (define out-port (open-output-bytes))
  (define err-port (open-output-bytes))
  (define proc (subprocess out-port in-port err-port
                            "openssl" "dgst" "-sha512" "-binary"))
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define stderr-bytes (get-output-bytes err-port))
  (close-input-port in-port)
  (close-output-port err-port)
  (cond
    [(zero? exit-code)
     (define digest (get-output-bytes out-port))
     (close-output-port out-port)
     digest]
    [else
     (close-output-port out-port)
     (error 'sha512-bytes
            (string-append "openssl sha512 failed: "
                           (bytes->string/utf-8 stderr-bytes)))]))

