#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 0)))

(paint (number->painter 0))
(paint diagonal-shading)

(paint-hires (below (beside diagonal-shading (rotate90 diagonal-shading)) (beside (rotate270 diagonal-shading) (rotate180 diagonal-shading))))

(paint einstein)