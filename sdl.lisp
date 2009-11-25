(require :sdl)
(require :sdl-demos)
(sdl-test:start)

(sdl:with-init ()
  (sdl:window 320 320)
  (loop for f from 0 upto (* 2 pi) by 0.1 do
       (sdl:draw-pixel-* (sdl:cast-to-int (+ (* (sin f) 200) 100))
			 (sdl:cast-to-int (+ (* (cos f) 200 ) 100))
			 :color sdl:*white*))
  (sdl:update-display)
  (sdl:with-events ()
    (:quit-event () t)
    (:video-expose-event () (sdl:update-display))))