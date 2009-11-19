#lang scheme

;all equivalent
(require (lib "scheme/date.ss"))
(require (lib "scheme/date"))
(require scheme/date)
(printf "Today is ~s\n" (date->string (seconds->date (current-seconds))))



;from plt  ; use "schematics"'s "random.plt" version 1.0 file "random.ss"
(require (planet schematics/random:1/random))
(require (planet "schematics/random:1/random"))
(require (planet "random.ss" ("schematics" "random.plt" 1)))
(display (random-gaussian))

(require "cake.ss") ;load a file in the current directory
(print-cake 2)