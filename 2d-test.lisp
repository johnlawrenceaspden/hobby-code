;;;; -*- Mode: Lisp -*-
;;;; Author: Matthew Danish <mrd@debian.org>
;;;; 2D graphics test
;;;;
;;;; Displays a BMP on the screen
;;;; You can click to move the BMP around, or use the A,S,D,W keys.
;;;; Q quits.

(defpackage #:2D-GRAPHICS-TEST
  (:use #:common-lisp)
  (:nicknames #:2dt)
  (:export #:start))

(in-package #:2D-GRAPHICS-TEST)

(defvar *bot-x* 270)
(defvar *bot-y* 190)
(defvar *bot-vx* 0)
(defvar *bot-vy* 0)
(defparameter *image* (sdl-data:data-file "cl-sdl" "bmp"))
(defvar *image-surface*)
(defvar *joysticks*)

(defun init-sdl ()
  (sdl:init (logior sdl:+init-video+))
  (let ((surface (sdl:set-video-mode 640 480 16
                                     (logior sdl:+resizable+
                                             sdl:+swsurface+
                                             sdl:+doublebuf+))))
    (when (sgum:null-pointer-p surface)
      (error "Unable to set video mode"))
    (sdl:wm-set-caption "SDL Test" nil)
    surface))

(defun open-joysticks ()
  (sdl:init-subsystem (logior sdl:+init-joystick+))
  (setq *joysticks* (make-array (sdl:num-joysticks)))
  (dotimes (i (sdl:num-joysticks))
    (let ((joystick (sdl:joystick-open i)))
      (when (sgum:null-pointer-p joystick)
        (warn "Unable to acquire joystick ~A" i))
      (setf (aref *joysticks* i) joystick))))

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
   (:joy-axis-motion (joystick axis value)
                     (format t "~&Joystick ~A axis ~A at ~A.~%" joystick axis value))
   (:joy-ball-motion (joystick ball xrel yrel)
                     (format t "~&Joystick ~A ball ~A moved (~A, ~A).~%" joystick ball xrel yrel))
   (:joy-hat-motion (joystick hat state)
                    (format t "~&Joystick ~A hat ~A at ~A.~%" joystick hat state))
   (:joy-button-up (joystick button)
                   (format t "~&Joystick ~A button ~A up.~%" joystick button))
   (:joy-button-down (joystick button)
                     (format t "~&Joystick ~A button ~A down.~%" joystick button))
   (:quit ()
          (return))
   (:resize (width height)
            (format t "Resized width = ~A height = ~A~%" width height))
   (:idle ()
          (funcall update-fn))))

(defun add-within-bounds (value delta lower-bound upper-bound)
  (max lower-bound (min upper-bound (+ value delta))))

(defun make-update-fn (surface)
  (let* ((num-rectangles 0) 
         (start-time (get-universal-time))
         (prev-time start-time))
    #'(lambda ()
        (cl-sdl:clear-screen surface :update-p nil)
        (sgum:with-foreign-objects ((rect sdl:rect))
          (setf (sdl:rect-x rect) *bot-x*
                (sdl:rect-y rect) *bot-y*)
          (sdl:blit-surface *image-surface* sgum:+null-pointer+
                            surface rect))
        (sdl:flip surface)

        (setf *bot-x* (add-within-bounds *bot-x* *bot-vx* 0 539)
              *bot-y* (add-within-bounds *bot-y* *bot-vy* 0 380))
        (incf num-rectangles)

        (let ((cur-time (get-universal-time)))
          (when (> (- cur-time prev-time) 1)
            (format t "~&Rectangles per second: ~A~%"
                    (float (/ num-rectangles (- cur-time prev-time))))
            (setf prev-time cur-time
                  num-rectangles 0))))))

(defun start ()
  (setf *bot-x* 270
        *bot-y* 190
        *bot-vx* 0
        *bot-vy* 0)
  (let ((*image-surface* (cl-sdl:load-bmp *image*)))
    (unwind-protect
         (let ((surface (init-sdl)))
           (open-joysticks)
           (run-sdl-event-loop surface (make-update-fn surface)))
      (progn
        (sdl:free-surface *image-surface*)
        (sdl:quit)))))
