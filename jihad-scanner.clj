;; The jihad scanner from let over lambda. Watch a communications channel for a word.

(defn make-scanner [ watchword ]
  (let [ curr (atom watchword) ]
    (fn [ chunk ]
      (doseq [c chunk]
        (if @curr
          (swap! curr (fn [cv] (cond (= (first cv) c)   (next cv) 
                                     (= (first watchword) c) (next watchword)
                                     :else watchword)))))
      (nil? @curr))))

;; I've added a clause so that jjihad still fires the detector. I think the original version doesn't catch jjihad
;; but does catch qjihad, which seems inconsistent.

;; Compared to the Common Lisp version, the requirement to wrap mutable state in an atom has cost us
;; a couple of spurious @s, and required "(swap! curr (fn [cv]" instead of "(setq" without really 
;; buying us anything. 

;; On the other hand, the fact that strings are just another form of seq has simplified the function
;; and generalised it to an arbitrary subsequence detector.


(let [a (make-scanner "jihad")]
  (a "We will start")
  (a "the jih")
  (a "ad tomorrow"))


(let [a (make-scanner "jihad")]
  (a "The jijihad starts tomorrow"))

(let [a (make-scanner "jihad")]
  (a "The jij ihad starts tomorrow"))

(let [a (make-scanner "jihad")]
  (a "Don't mention the j-word"))

(let [a (make-scanner '(97 98 99))]
  (for [x (partition 3 (range 110))] (a x)))

(let [a (make-scanner '[a b c])]
  (for [ x '((a b) (c d)) ] (a x)))

(let [a (make-scanner '[a b c])]
  (for [ x '((a b) [c d]) ] (a x)))