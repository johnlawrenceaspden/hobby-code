;; The classic ZX Spectrum sine drawing program

(use 'simple-plotter)
(create-window "ZX Graphics" 256 176)

;; 10 FOR X = 1 TO 255
;; 20 PLOT X, 88+80*SIN(X/128*PI)
;; 30 NEXT X

(doseq [x (range 256)]
  (plot x (+ 88 (* 80 (Math/sin (* Math/PI (/ x 128)))))))

;; Hey, let's have some axes as well

;; 40 INK 6
;; 50 PLOT 0,88: DRAW 255,0
;; 60 PLOT 127,0: DRAW 0,168

(ink gray)
(plot 0 88) (draw 255 0)
(plot 127 0) (draw 0 175)















