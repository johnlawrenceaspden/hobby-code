;; Someone's proposal for calculating air density. Looks wrong to me!

(def T 0) ;; temperature in centigrade
(def H 0.00) ;; humidity

(def R-dry-air 287.058) ;; specific gas constant for dry air (J/kg/K)

(def absolute-temperature (+ T 273.25)) 
absolute-temperature ; 273.25


(def saturation-pressure (+ (* 5 T T) (* -52.2 T) 1319.9))

saturation-pressure ; 1319.9 ; 1319.9 (millibars?) 

(def vapour-pressure (* H saturation-pressure)) ; #'user/vapour-pressure

vapour-pressure ; 0.0 ; 0.0

(def air-pressure (- saturation-pressure vapour-pressure)) ; #'user/air-pressure

air-pressure ; 1319.9 ; 1319.9


(def density (+ (/ air-pressure    (* R-dry-air absolute-temperature ))
                (/ vapour-pressure (* 461.5 absolute-temperature     ))   )) ; #'user/density

density ; 0.016827174697671327 ; 0.016827174697671327

;; one millibar is 100 Pascals
;; Note one standard atmosphere is 101.325 kPa = 101325 N/m^2 (ten tons/ square meter!!) 1kg/sq cm
;; The saturation vapour pressure of water at 0C is 0.6105 N/m^2 (http://www.engineeringtoolbox.com/water-vapor-saturation-pressure-d_599.html)

