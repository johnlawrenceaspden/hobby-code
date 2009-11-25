;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; 2D graphics test
;;;;
;;;; Displays a rectangle on the screen, changing colors.
;;;; You can click to move the rectangle around, or use the A,S,D,W keys.
;;;; Q quits.

(require :sdl)

(defpackage #:sdl-example1
  (:use #:common-lisp #:sdl)
  (:nicknames #:sdl-ex1)
  (:export #:start))

(in-package #:sdl-example1)

(defvar *screen-x* 1280)
(defvar *screen-y* 960)
(defvar *size* 10)

(defvar *bot-x* (/(- *screen-x* *size*)2))
(defvar *bot-y* (/(- *screen-y* *size*)2))
(defvar *bot-vx* 0)
(defvar *bot-vy* 0)

(defun init-sdl ()
  (sdl:init (logior sdl:+init-video+))
  (let ((surface (sdl:set-video-mode *screen-x* *screen-y* 16
				     (logior sdl:+resizable+
					     sdl:+swsurface+))))
    (when (sgum:null-pointer-p surface)
      (error "Unable to set video mode"))
    (sdl:wm-set-caption "SDL Example 1" nil)
    surface))

(defun run-sdl-event-loop (surface update-fn)
  (sdl:event-loop
   (:key-down (scan-code key mod unicode)
	      (cond ((= key (char-code #\q))
		     (return))
		    ((= key (char-code #\w))
		     (setf *bot-vy* -1))
		    ((= key (char-code #\s))
		     (setf *bot-vy* 1))
		    ((= key (char-code #\d))
		     (setf *bot-vx* 1))
		    ((= key (char-code #\a))
		     (setf *bot-vx* -1))))
   (:key-up (scan-code key mod unicode)
	    (cond ((= key (char-code #\q))
		   (return))
		  ((= key (char-code #\w))
		   (setf *bot-vy* 0))
		  ((= key (char-code #\s))
		   (setf *bot-vy* 0))
		  ((= key (char-code #\d))
		   (setf *bot-vx* 0))
		  ((= key (char-code #\a))
		   (setf *bot-vx* 0))))
   (:mouse-button-up (button x y)
		     (format t "Mouse button up: ~A (~A, ~A)~%" button x y)
		     (setf *bot-x* x
			   *bot-y* y))
   (:mouse-button-down (button x y)
		       (format t "Mouse button dn: ~A (~A, ~A)~%" button x y))
   (:quit ()
	  (return))
   (:resize (width height)
	    (format t "Resized width = ~A height = ~A~%" width height))
   (:idle ()
	  (funcall update-fn))))

(defun add-within-bounds (value delta lower-bound upper-bound)
  (max lower-bound (min upper-bound (+ value delta))))

(defun random-value ()
  (- (random 3) 1))

(defun make-update-fn (surface)
  (let ((r 128) (g 128) (b 128)
	(num-rectangles 0) (start-time (get-universal-time))
	(prev-time nil))
    (setf prev-time start-time)
    #'(lambda ()
	(loop repeat 1 do
             (sdl:draw-filled-rectangle surface *bot-x* *bot-y* *size* *size* r g b)
             (sdl:update-rect surface *bot-x* *bot-y* (+ 1 *size*) (+ 1 *size*))
             (setf r (add-within-bounds r (random-value) 50 255)
                   g (add-within-bounds b (random-value) 50 255)
                   b (add-within-bounds g (random-value) 50 255)
                   *bot-x* (add-within-bounds *bot-x* *bot-vx* 0 (- *screen-x* *size* 1))
                   *bot-y* (add-within-bounds *bot-y* *bot-vy* 0 (- *screen-y* *size* 1)))
             (incf num-rectangles)
	(let ((cur-time (get-universal-time)))
	  (when (> (- cur-time prev-time) 1)
	    (setf prev-time cur-time)
	    (format t "~&Rectangles per second: ~A~% ~A ~A ~A ~A~%"
		    (float (/ num-rectangles (- cur-time start-time)))
                    *bot-x* *bot-vx* *bot-y* *bot-vy*)))))))

(defun start ()
  (setf *bot-x* 270
	*bot-y* 190
	*bot-vx* 0
	*bot-vy* 0)
  (unwind-protect
      (progn
	(let ((surface (init-sdl)))
	  (run-sdl-event-loop surface (make-update-fn surface))))
    (sdl:quit)))
