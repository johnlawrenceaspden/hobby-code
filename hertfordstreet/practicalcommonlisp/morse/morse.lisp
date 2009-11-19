(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :morse
  (:use :COMMON-LISP))

(in-package :morse)

(defparameter *morse-mapping*
  '((#\A   ".-")
    (#\B   "-...")
    (#\C   "-.-.")
    (#\D   "-..")
    (#\E   ".")
    (#\F   "..-.")
    (#\G   "--.")
    (#\H   "....")
    (#\I   "..")
    (#\J   ".---")
    (#\K   "-.-")
    (#\L   ".-..")
    (#\M   "--")
    (#\N   "-.")
    (#\O   "---")
    (#\P   ".--.")
    (#\Q   "--.-")
    (#\R   ".-.")
    (#\S   "...")
    (#\T   "-")
    (#\U   "..-")
    (#\V   "...-")
    (#\W   ".--")
    (#\X   "-..-")
    (#\Y   "-.--")
    (#\Z   "--..")
    (#\0 "  -----")
    (#\1   ".----")
    (#\2   "..---")
    (#\3   "...--")
    (#\4   "....-")
    (#\5   ".....")
    (#\6   "-....")
    (#\7   "--...")
    (#\8   "---..")
    (#\9   "----.")
    (#\.  ".-.-.-")q
    (#\, "--..--")
    (#\? "..--..")))

(defun character-to-morse (character)
  (cadr (assoc character *morse-mapping* :test #'char-equal)))

(defun morse-to-character (morse-string)
  (first (find morse-string *morse-mapping* :test #'string= :key #'cadr)))

(defun string-to-morse (string)
  (with-output-to-string (morse)
    (write-string (character-to-morse (aref string 0)) morse)
    (loop
     for char across (subseq string 1)
     do (write-char #\Space morse)
     do (write-string (character-to-morse char) morse))))

(defun morse-to-string (string encoding)
  (with-output-to-string (character-stream)
    (loop
     for morse-char in (split-sequence:split-sequence #\Space string
						      :remove-empty-subseqs t)
     do (write-char (morse-to-character morse-char) character-stream))))
