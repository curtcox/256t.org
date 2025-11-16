#lang racket

(require racket/file
         racket/list
         "cid.rkt")

(define (write-cid-file destination content)
  (call-with-output-file* destination
    #:exists 'replace
    #:mode 'binary
    (lambda (out)
      (write-bytes content out))))

(define (main)
  (make-directory* cids-dir)
  (define examples
    (sort (directory-list examples-dir #:build? #t)
          string<?
          #:key path->string))
  (for ([path (in-list examples)])
    (unless (directory-exists? path)
      (define content (file->bytes path))
      (define cid (compute-cid content))
      (define destination (build-path cids-dir cid))
      (write-cid-file destination content)
      (printf "Wrote ~a from ~a\n"
              (path->string (file-name-from-path destination))
              (path->string (file-name-from-path path)))))
  0)

(module+ main
  (exit (main)))
