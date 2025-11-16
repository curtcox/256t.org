#lang racket

(require racket/file
         racket/list
         "cid.rkt")

(define (main)
  (define mismatches '())
  (define count 0)
  (for ([path (in-list (sort (directory-list cids-dir #:build? #t)
                             string<?
                             #:key path->string))])
    (unless (directory-exists? path)
      (set! count (add1 count))
      (define actual (path->string (file-name-from-path path)))
      (define expected (compute-cid (file->bytes path)))
      (unless (string=? actual expected)
        (set! mismatches (cons (cons actual expected) mismatches)))))
  (if (null? mismatches)
      (begin
        (printf "All ~a CID files match their contents.\n" count)
        0)
      (begin
        (printf "Found CID mismatches:\n")
        (for ([entry (in-list (reverse mismatches))])
          (printf "- ~a should be ~a\n" (car entry) (cdr entry)))
        1)))

(module+ main
  (exit (main)))
