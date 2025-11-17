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
  ;; subprocess returns ports in order: stdout (input), stdin (output), stderr (input)
  (define-values (stdout-port stdin-port stderr-port proc)
    (subprocess #f #f #f
                "openssl" "dgst" "-sha512" "-binary"))
  (write-bytes content stdin-port)
  (close-output-port stdin-port)
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define stderr-bytes (port->bytes stderr-port))
  (close-input-port stderr-port)
  (cond
    [(zero? exit-code)
     (define digest (port->bytes stdout-port))
     (close-input-port stdout-port)
     digest]
    [else
     (close-input-port stdout-port)
     (error 'sha512-bytes
            (string-append "openssl sha512 failed: "
                           (bytes->string/utf-8 stderr-bytes)))]))

