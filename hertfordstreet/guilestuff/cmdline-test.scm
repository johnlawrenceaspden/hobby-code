#! /usr/bin/env guile
!#


(use-modules (ice-9 getopt-long))
(write (command-line))
(newline)

(define option-spec
  '((version (single-char #\v) (value #f))
    (help    (single-char #\h) (value #f))))


(define options (getopt-long (command-line) option-spec))

(write options)
(newline)
