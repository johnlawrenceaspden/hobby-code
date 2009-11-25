;;;; -*- Mode: Lisp -*-

(defpackage #:sdl-data
  (:use #:common-lisp)
  (:export #:data-file))

(in-package #:sdl-data)

(defparameter *possible-data-path*
  (let ((file (or *load-truename* *compile-file-truename*)))
    (when file
      (merge-pathnames
       (make-pathname :directory '(:relative "data"))
       (make-pathname :directory (pathname-directory file))))))

;; Search directories
(defparameter *data-search-dirs*
  `((,(pathname-device *possible-data-path*)
     ,@(pathname-directory *possible-data-path*))
    #+unix (nil :absolute "usr" "share" "cl-sdl-demos" "data")
    #+unix (nil :absolute "usr" "local" "share" "cl-sdl-demos" "data")
    #+unix (nil :absolute "usr" "local" "cl-sdl" "data")
    #+unix (nil :absolute "opt" "cl-sdl" "data")
    (nil :relative "data")
    (nil :relative "examples" "data")
    (nil :relative :up "data")
    #+win32 ("C" :absolute "CL-SDL" "DATA")
    #+win32 ("C" :absolute "CL" "PACKAGES" "CL-SDL" "EXAMPLES" "DATA")))

(defun find-data-path (&rest args)
  (flet ((try (device dir)
           (probe-file (apply #'make-pathname 
			      :directory dir 
			      (if device
                                  (list* #+allegro :device
                                         #+lispworks :host
                                         device args)
                                  args)))))
    (dolist (dir *data-search-dirs* nil)
      (let ((file (try (first dir) (rest dir))))
        (when file (return file))))))

#+use-lpn
(setf (logical-pathname-translations "sdl-data")
      `(("**;*.*.*" ,(find-data-path))))

#-use-lpn
(defparameter *data-dir*
  (pathname-directory (probe-file (find-data-path))))

(defun data-file (name type)
  (namestring
   #+use-lpn
   (translate-logical-pathname
    (make-pathname #+use-lpn :host #+use-lpn "sdl-data"
                   #-use-lpn :directory #-use-lpn *data-dir*
                   :name #+use-lpn (string-upcase name) #-use-lpn name
                   :type #+use-lpn (string-upcase type) #-use-lpn type))
   #-use-lpn
   (find-data-path :name name :type type)))

