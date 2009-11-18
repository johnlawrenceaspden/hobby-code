;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
(def starts (map (fn[tile][[tile 0]]) tiles))
(map ptilelist starts)
(count starts)
(def it1 (mapcat tilelist->longertilelists starts))
(map ptilelist it1)
(count it1)
(def it2 (mapcat tilelist->longertilelists it1))
(count it2)
(map ptilelist it2)
(def it3 (mapcat tilelist->longertilelists it2))
(count it3)
(map ptilelist it3)
(def it4 (mapcat tilelist->longertilelists it3))
(count it4)
(map ptilelist it4)
(def it5 (mapcat tilelist->longertilelists it4))
(count it5)
(map ptilelist it5)
(def it6 (mapcat tilelist->longertilelists it5))
(count it6)
(map ptilelist it6)
(def it7 (mapcat tilelist->longertilelists it6))
(count it7)
(map ptilelist it7)
(def it8 (mapcat tilelist->longertilelists it7))
(count it8)
(map ptilelist it8)
(def it9 (mapcat tilelist->longertilelists it8))
(count it9)
(map ptilelist it9)
(def it10 (mapcat tilelist->longertilelists it9))
(count it10)
(map ptilelist it10)
(def it11 (mapcat tilelist->longertilelists it10))
(count it11)
(map ptilelist it10)



(def i0 [[tile0 0]])
(def ic (drop 1 tiles))

(def candidates (for [tile ic rotation (range 6)] [tile rotation]))

(def compat (filter (fn [[tile rotation]](check-consistency i0 tile rotation numericadjlist)) candidates))

(count compat)

(map (fn [[tile rotation]] [(tile :number) rotation]) compat)

(set tiles)

(map (fn[tile] (map #(check-consistency i0 tile  % numericadjlist) (range 6))) tiles)

(check-consistency i0 tile2 3 numericadjlist)
(check-consistency i0 tile5 3 numericadjlist)
(check-consistency i0 tile7 3 numericadjlist)
(check-consistency i0 tile9 3 numericadjlist)

(def i1 [[tile0 0][tile2 3]])
(def i2 [[tile0 0][tile5 3]]) 
(def i3 [[tile0 0][tile7 3]])
(def i4 [[tile0 0][tile9 3]])

(map (fn[tile] (map #(check-consistency i1 tile  % numericadjlist) (range 6))) tiles)
(map (fn[tile] (map #(check-consistency i2 tile  % numericadjlist) (range 6))) tiles)
(map (fn[tile] (map #(check-consistency i3 tile  % numericadjlist) (range 6))) tiles)
(map (fn[tile] (map #(check-consistency i4 tile  % numericadjlist) (range 6))) tiles)
(map (fn[tile] (map #(check-consistency i5 tile  % numericadjlist) (range 6))) tiles)
)